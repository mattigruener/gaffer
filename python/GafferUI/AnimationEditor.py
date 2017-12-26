##########################################################################
#
#  Copyright (c) 2017, Matti Gruener. All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions are
#  met:
#
#      * Redistributions of source code must retain the above
#        copyright notice, this list of conditions and the following
#        disclaimer.
#
#      * Redistributions in binary form must reproduce the above
#        copyright notice, this list of conditions and the following
#        disclaimer in the documentation and/or other materials provided with
#        the distribution.
#
#      * Neither the name of John Haddon nor the names of
#        any other contributors to this software may be used to endorse or
#        promote products derived from this software without specific prior
#        written permission.
#
#  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
#  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
#  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
#  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
#  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
#  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
#  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
#  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
#  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
#  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
#  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
##########################################################################
import IECore

import Gaffer
import GafferScene
import GafferUI

from Qt import QtWidgets

def plugIsAnimated( plug ) :

    input = plug.getInput()
    if not input :
        return False

    if input.ancestor( Gaffer.Animation ) :
        return True

    return False


class AnimationPathFilter( Gaffer.PathFilter ) :

    def __init__( self, scriptNode, userData = {}, selection = None ) :

        Gaffer.PathFilter.__init__( self, userData )

        self.__scriptNode = scriptNode
        self.__selection = selection or []

    def setSelection( self, selection ) :
        self.__selection = selection

        self.changedSignal()( self )

    def __hasAnimatedChild( self, graphComponent ) :

        for child in graphComponent.children() :
            if isinstance( child, Gaffer.Plug ) :
                if plugIsAnimated( child ) :
                    return True
                else:
                    if self.__hasAnimatedChild( child ) :
                        return True

        return False

    def _filter( self, paths ) :

        result = []

        for path in paths :
            descendant = self.__scriptNode.descendant( str( path )[1:].replace('/', '.') )

            for selected in self.__selection :

                if not descendant == selected and not selected.isAncestorOf( descendant ) :
                    continue

                if isinstance( descendant, Gaffer.Node ) or isinstance( descendant, Gaffer.Plug ) :
                    if isinstance( descendant, Gaffer.Plug ) and plugIsAnimated( descendant ) :
                        result.append( path )
                    elif self.__hasAnimatedChild( descendant ) :
                        result.append( path )

        return result


class AnimationEditor( GafferUI.NodeSetEditor ) :

    def __init__( self, scriptNode, **kw ) :

        self.__main = GafferUI.ListContainer( GafferUI.ListContainer.Orientation.Horizontal )

        GafferUI.NodeSetEditor.__init__( self, self.__main, scriptNode, **kw )

        self.__scriptNode = scriptNode

        self.__animationFilter = AnimationPathFilter( scriptNode )
        self.__curveList = GafferUI.PathListingWidget(
            Gaffer.GraphComponentPath( scriptNode, '/', filter = self.__animationFilter ),
            columns = ( GafferUI.PathListingWidget.defaultNameColumn, ),
            displayMode = GafferUI.PathListingWidget.DisplayMode.Tree,
			allowMultipleSelection=True )

        # \todo: set proper size for the curveList widget
        self.__curveList._qtWidget().setSizePolicy( QtWidgets.QSizePolicy.Preferred, QtWidgets.QSizePolicy.Preferred )
        self.__expansionChangedConnection = self.__curveList.expansionChangedSignal().connect( Gaffer.WeakMethod( self.__expansionChanged ) )
        self.__selectionChangedConnection = self.__curveList.selectionChangedSignal().connect( Gaffer.WeakMethod( self.__selectionChanged ) )

        self.__gadgetWidget = GafferUI.GadgetWidget(
            bufferOptions = set( [ GafferUI.GLWidget.BufferOptions.Double, ] ), )
        self.__gadgetWidget._qtWidget().setSizePolicy( QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Expanding )

        self.__animationGadget = GafferUI.AnimationGadget()

        self.__gadgetWidget.getViewportGadget().setPrimaryChild( self.__animationGadget )
        self.__gadgetWidget.getViewportGadget().setDragTracking( True )
        self.__gadgetWidget.getViewportGadget().setVariableAspectZoom( True )

        # self.__main.addChild( self.__curveNames )
        self.__main.addChild( self.__curveList )
        self.__main.addChild( self.__gadgetWidget )

        self._updateFromSet()
        self._updateFromContext( [ "frame" ] )

        self.__keyPressConnection = self.__gadgetWidget.keyPressSignal().connect( Gaffer.WeakMethod( self.__keyPress ) )

        self.__visiblePlugs = self.__editablePlugs = None

        # TODO: initial framing
        bound = IECore.Box3f( IECore.V3f( -1, -1, 0 ), IECore.V3f( 10, 10, 0 ) )
        self.__gadgetWidget.getViewportGadget().frame( bound )

    def _updateFromSet( self ) :

        GafferUI.NodeSetEditor._updateFromSet( self )

        nodeList = list( self.getNodeSet() )
        self.__animationFilter.setSelection( nodeList )

    def _updateFromContext( self, modifiedItems ) :

        if "frame" not in modifiedItems :
            return

        self.__animationGadget.setFrame( self.getContext().getFrame() )

    def __keyPress( self, widget, event ) :

        pass

    def __expansionChanged( self, pathListing ) :

        assert( pathListing is self.__curveList )

        paths = pathListing.getExpandedPaths()

        plugList = []

        for path in paths:
            graphComponent = self.__scriptNode.descendant( str( path ).replace( '/', '.' ) )
            for child in graphComponent.children() :
                if isinstance( child, Gaffer.Plug ) and plugIsAnimated( child ) :
                    plugList.append( child )

        self.__visiblePlugs = set( plugList )

        self.__animationGadget.setVisiblePlugs( self.__visiblePlugs )
        self.__animationGadget.setEditablePlugs( ( self.__editablePlugs or set() ) & self.__visiblePlugs )

    def __selectionChanged( self, pathListing ) :

        assert( pathListing is self.__curveList )

        paths = pathListing.getSelectedPaths()

        plugList = []

        for path in paths :
            graphComponent = self.__scriptNode.descendant( str( path ).replace( '/', '.' ) )

            if isinstance( graphComponent, Gaffer.Plug ) and plugIsAnimated( graphComponent ) :
                plugList.append( graphComponent )

            for child in graphComponent.children() :
                if isinstance( child, Gaffer.Plug ) and plugIsAnimated( child ) :
                    plugList.append( child )

        self.__editablePlugs = set( plugList )

        self.__animationGadget.setEditablePlugs( self.__editablePlugs & self.__visiblePlugs )


GafferUI.EditorWidget.registerType( "AnimationEditor", AnimationEditor )
