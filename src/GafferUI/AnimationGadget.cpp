//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2017, Matti Gruener. All rights reserved.
//
//  Redistribution and use in source and binary forms, with or without
//  modification, are permitted provided that the following conditions are
//  met:
//
//      * Redistributions of source code must retain the above
//        copyright notice, this list of conditions and the following
//        disclaimer.
//
//      * Redistributions in binary form must reproduce the above
//        copyright notice, this list of conditions and the following
//        disclaimer in the documentation and/or other materials provided with
//        the distribution.
//
//      * Neither the name of John Haddon nor the names of
//        any other contributors to this software may be used to endorse or
//        promote products derived from this software without specific prior
//        written permission.
//
//  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
//  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
//  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
//  PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
//  CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
//  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
//  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
//  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
//  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
//  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
//////////////////////////////////////////////////////////////////////////
#include "math.h"
#include "boost/bind.hpp"
#include "boost/algorithm/string.hpp"

#include "IECore/NullObject.h"
#include "IECore/InternedString.h"

#include "Gaffer/Node.h"
#include "Gaffer/Plug.h"
#include "Gaffer/StandardSet.h"
#include "Gaffer/Animation.h"

#include "GafferUI/AnimationGadget.h"
#include "GafferUI/Style.h"
#include "GafferUI/Pointer.h"

using namespace Gaffer;
using namespace GafferUI;
using namespace Imath;

namespace
{

// \todo: probably needs a way to determine fps at some point
float frameToTime( float frame )
{
	return frame / 24.0;
}

float timeToFrame( float time )
{
	return time * 24.0;
}

} // namespace

//////////////////////////////////////////////////////////////////////////
// AnimationGadget implementation
//////////////////////////////////////////////////////////////////////////

IE_CORE_DEFINERUNTIMETYPED( AnimationGadget );

AnimationGadget::AnimationGadget()
	: m_viewportGadget( nullptr ), m_currentFrame( 0 ), m_dragStartPosition( 0 ), m_lastDragPosition( 0 ), m_dragMode( DragMode::None ), m_moveAxis( MoveAxis::Both )
{
	parentChangedSignal().connect( boost::bind( &AnimationGadget::parentChanged, this, ::_1, ::_2 ) );

	buttonPressSignal().connect( boost::bind( &AnimationGadget::buttonPress, this, ::_1,  ::_2 ) );
	buttonReleaseSignal().connect( boost::bind( &AnimationGadget::buttonRelease, this, ::_1,  ::_2 ) );

	keyPressSignal().connect( boost::bind( &AnimationGadget::keyPress, this, ::_1,  ::_2 ) );

	dragBeginSignal().connect( boost::bind( &AnimationGadget::dragBegin, this, ::_1, ::_2 ) );
	dragEnterSignal().connect( boost::bind( &AnimationGadget::dragEnter, this, ::_1, ::_2 ) );
	dragMoveSignal().connect( boost::bind( &AnimationGadget::dragMove, this, ::_1, ::_2 ) );
	dragEndSignal().connect( boost::bind( &AnimationGadget::dragEnd, this, ::_1, ::_2 ) );
}

AnimationGadget::~AnimationGadget()
{
}

void AnimationGadget::doRenderLayer( Layer layer, const Style *style ) const
{
	Gadget::doRenderLayer( layer, style );

	glDisable( GL_DEPTH_TEST );

	AxisDefinition xAxis, yAxis;
	computeGrid( xAxis, yAxis );

	Imath::V2i resolution = m_viewportGadget->getViewport();

	ViewportGadget::RasterScope rasterScope( m_viewportGadget );

	switch ( layer )
	{

	case AnimationLayer::Grid :
	{
		// drawing base grid
		for( const auto &x : xAxis.main )
		{
			style->renderLine( IECore::LineSegment3f( V3f( x.first, 0, 0 ), V3f( x.first, resolution.y, 0 ) ), x.second == 0.0f ? 3.0 : 2.0 );
		}

		for( const auto &y : yAxis.main )
		{
			style->renderLine( IECore::LineSegment3f( V3f( 0, y.first, 0 ), V3f( resolution.x, y.first, 0 ) ), y.second == 0.0f ? 3.0 : 2.0 );
		}

		// drawing sub grid for frames
		for( float x : xAxis.secondary )
		{
			style->renderLine( IECore::LineSegment3f( V3f( x, 0, 0 ), V3f( x, resolution.y, 0 ) ), 1.0 );
		}

		break;
	}

	case AnimationLayer::Curves :
	{
		for( auto &curvePlug : m_curvePlugsEditable )
		{
			const Animation::CurvePlug::Keys &keys = curvePlug->keys();
			for( Animation::CurvePlug::Keys::const_iterator it = keys.begin(); it != keys.end(); ++it )
			{
				const Animation::Key &key = (*it);
				if( m_selectedKeys.count( UniqueKey( key, curvePlug ) ) > 0 )
				{
					continue;
				}

				V2f keyPosition = m_viewportGadget->worldToRasterSpace( V3f( key.time, key.value, 0 ) );
				style->renderKeyFrame( keyPosition, Style::NormalState );
			}
		}

		break;

	}

	case AnimationLayer::Highlighting :
	{
		for( auto &curvePlug : m_curvePlugsVisible )
		{
			const Animation::CurvePlug::Keys &keys = curvePlug->keys();
			for( Animation::CurvePlug::Keys::const_iterator it = keys.begin(); it != keys.end(); ++it )
			{
				const Animation::Key &key = (*it);
				if( m_selectedKeys.count( UniqueKey( key, curvePlug ) ) == 0 )
				{
					continue;
				}

				V2f keyPosition = m_viewportGadget->worldToRasterSpace( V3f( key.time, key.value, 0 ) );
				style->renderKeyFrame( keyPosition, Style::HighlightedState );
			}
		}

		break;
	}


	case AnimationLayer::Axes :
	{
		// draw frame indication
		int currentFrameRasterPosition = m_viewportGadget->worldToRasterSpace( V3f( m_currentFrame / 24.0, 0, 0 ) ).x;
		style->renderLine( IECore::LineSegment3f( V3f( currentFrameRasterPosition, 0, 0 ), V3f( currentFrameRasterPosition, resolution.y, 0 ) ), 2.0, new Imath::Color3f( 1.0, 0.2, 0.2 ) );

		// drawing axes on top of everything
		// \todo: store colors and sizes somewhere a bit more global
		glColor4f( 38.0 / 255, 38.0 / 255, 38.0 / 255, 1.0 );
		style->renderSolidRectangle( Box2f( V2f( 0 ) , V2f( 60, resolution.y - 20 ) ) );
		style->renderSolidRectangle( Box2f( V2f( 0, resolution.y - 20 ) , V2f( resolution.x, resolution.y ) ) );

		boost::format format( "%.2f" );
		int textScale = 10;

		// \todo Clean up code duplication.
		// \todo There's some potential here to reduce pushing/popping of matrices by pulling those out of the loop

		for( const auto &x : xAxis.main )
		{
			if( x.first < 60 )
			{
				continue;
			}

			glPushMatrix();

			std::string label = boost::str( format % x.second );

			Box3f labelBound = style->textBound( Style::BodyText, label );

			glTranslatef( x.first - labelBound.center().x * textScale, resolution.y-5, 0.0f );
			glScalef( textScale, -textScale, textScale );

			style->renderText( Style::BodyText, label );

			glPopMatrix();
		}

		for( const auto &y : yAxis.main )
		{

			if( y.first > resolution.y - 20 )
			{
				continue;
			}

			glPushMatrix();

			std::string label = boost::str( format % y.second );

			Box3f labelBound = style->textBound( Style::BodyText, label );

			glTranslatef( 55 - labelBound.size().x * textScale, y.first + labelBound.center().y * textScale, 0.0f );
			glScalef( textScale, -textScale, textScale );

			style->renderText( Style::BodyText, label );

			glPopMatrix();
		}

		break;

	}

	case AnimationLayer::Overlay :
	{
		if( m_dragMode == DragMode::Selecting )
		{
			Box2f b;
			b.extendBy( m_viewportGadget->gadgetToRasterSpace( V3f( m_dragStartPosition.x, m_dragStartPosition.y, 0 ), this ) );
			b.extendBy( m_viewportGadget->gadgetToRasterSpace( V3f( m_lastDragPosition.x, m_lastDragPosition.y, 0 ), this ) );
			style->renderSelectionBox( b );
		}
	}

	default:
		break;

	}
}

void AnimationGadget::parentChanged( GraphComponent *child, GraphComponent *oldParent )
{
	m_viewportGadget = ancestor<ViewportGadget>();

	if( !m_viewportGadget )
	{
		// \todo: not sure what to do here. this happens when closing gaffer as well.
		return;
	}

	m_viewportGadget->cameraChangedSignal().connect( boost::bind( &AnimationGadget::cameraChanged, this ) );
	m_viewportGadget->viewportChangedSignal().connect( boost::bind( &AnimationGadget::viewportChanged, this ) );

	requestRender();
}

// \TODO: are these still needed?
void AnimationGadget::cameraChanged()
{
	requestRender();
}

void AnimationGadget::viewportChanged()
{
	requestRender();
}

void AnimationGadget::computeGrid( AxisDefinition &x, AxisDefinition &y ) const
{
	Imath::V2i resolution = m_viewportGadget->getViewport();

	IECore::LineSegment3f min, max;
	min = m_viewportGadget->rasterToWorldSpace( V2f( 0 ) );
	max = m_viewportGadget->rasterToWorldSpace( V2f( resolution.x, resolution.y ) );
	Imath::Box2f viewportBounds = Box2f( V2f( min.p0.x, min.p0.y ), V2f( max.p0.x, max.p0.y ) );

	Box2f viewportBoundsFrames( viewportBounds.min * 24, viewportBounds.max * 24 );
	V2i labelMinSize( 50, 20 );
	int xStride = 1;
	int yStride = 1;

	// \todo the box's size() is unrealiable because it considers the box empty for the inverted coords we seem to have here
	V2f pxPerUnit = V2f(
		resolution.x / abs( viewportBoundsFrames.min.x - viewportBoundsFrames.max.x ),
		resolution.y / abs( viewportBounds.min.y - viewportBounds.max.y ) );

	if( pxPerUnit.x < labelMinSize.x )
	{
		xStride = 5;
		pxPerUnit.x *= 5;

		while( pxPerUnit.x < labelMinSize.x && pxPerUnit.x != 0 )
		{
			xStride *= 10;
			pxPerUnit.x *= 10;
		}
	}

	if( pxPerUnit.y < labelMinSize.y )
	{
		yStride = 5;
		pxPerUnit.y *= 5;

		while( pxPerUnit.y < labelMinSize.y && pxPerUnit.y != 0 )
		{
			yStride *= 10;
			pxPerUnit.y *= 10;
		}
	}

	for( int i = std::ceil( viewportBoundsFrames.min.x ) - xStride; i < std::ceil( viewportBoundsFrames.max.x ); ++i )
	{
		if( xStride == 1 || ( i % xStride ) == 0 )
		{
			float frame = i / 24.0;
			x.main.push_back( std::make_pair( m_viewportGadget->worldToRasterSpace( V3f( frame, 0, 0 ) ).x, i ) );

			float subStride = xStride / ( 5.0 * 24 );
			for( int s = 1; s < 5; ++s )
			{
				x.secondary.push_back( m_viewportGadget->worldToRasterSpace( V3f( frame + s * subStride, 0, 0 ) ).x );
			}
		}
	}

	for( int j = std::ceil( viewportBounds.max.y ) - yStride; j < std::ceil( viewportBounds.min.y ); ++j )
	{
		if( yStride == 1 || ( j % yStride ) == 0 )
		{
			y.main.push_back( std::make_pair( m_viewportGadget->worldToRasterSpace( V3f( 0, j, 0 ) ).y, j ) );
		}
	}

}

void AnimationGadget::setVisiblePlugs( const std::vector<Gaffer::Plug *> &plugs )
{
	m_curvePlugsVisible.clear();

	for( GadgetPtr gadget : m_animationCurves )
	{
		CurveGadgetPtr curveGadget = IECore::runTimeCast<CurveGadget>( gadget.get() );
		if( curveGadget )
		{
			removeChild( curveGadget );
		}
	}
	// all these pointers are invalid now anyway
	m_animationCurves.clear();

	for( const auto &plug : plugs )
	{
		if( Gaffer::Animation::CurvePlug *curvePlug = IECore::runTimeCast<Gaffer::Animation::CurvePlug>( plug->getInput()->parent() ) )
		{
			std::string name = plug->fullName();  // \todo: clean up name
			CurveGadget *curveGadget = new CurveGadget( name, curvePlug );
			addChild( curveGadget );
			m_animationCurves.push_back( curveGadget );

			m_curvePlugsVisible.push_back( curvePlug );
		}
	}

	requestRender();
}

void AnimationGadget::setEditablePlugs( const std::vector<Gaffer::Plug *> &plugs )
{
	m_curvePlugsEditable.clear();

	// \todo: also needs to modify existing selection

	for( auto &plug : plugs )
	{
		if( Gaffer::Animation::CurvePlug *curvePlug = IECore::runTimeCast<Gaffer::Animation::CurvePlug>( plug->getInput()->parent() ) )
		{
			m_curvePlugsEditable.push_back( curvePlug );
		}
	}

	requestRender();
}

void AnimationGadget::setFrame( float frame )
{
	m_currentFrame = frame;

	requestRender();
}

void AnimationGadget::insertKeyframes()
{
	for( auto &curvePlug : m_curvePlugsEditable )
	{
		float time = frameToTime( m_currentFrame );
		if( !curvePlug->hasKey( time ) )
		{
			float value = curvePlug->evaluate( time );
			curvePlug->addKey( Animation::Key( time, value ) );
		}
	}

	requestRender();
}

void AnimationGadget::removeKeyframes()
{
	for( const auto &i : m_selectedKeys )
	{
		i.second->removeKey( i.first.time );
	}

	requestRender();
}

// \todo: This seems a bit messy and it doesn't support Undo.
// Not sure if we'd ideally hold pointers to the Keys instead to edit them in-place?
void AnimationGadget::moveKeyframes( const V2f offset )
{

	std::set<UniqueKey> tmp;

	for( auto &uniqueKey : m_selectedKeys )
	{
		// create copy of Key from drag initiation
		Animation::Key key = m_dragInitialKeys[uniqueKey];

		// remove current key from the curve as it will need to change
		uniqueKey.second->removeKey( uniqueKey.first.time );

		// apply offsets
		if( m_moveAxis == MoveAxis::Both || m_moveAxis == MoveAxis::Y )
		{
			key.value += offset.y;
		}

		if( m_moveAxis == MoveAxis::Both || m_moveAxis == MoveAxis::X )
		{
			float frame = round( timeToFrame( key.time + offset.x ) );
			key.time = frameToTime( frame );
		}

		// add Key back to curve
		if( uniqueKey.second->hasKey( key.time ) )
		{
			// protect existing keys by adding small epsilon
			// \todo store epsilon somewhere more global
			key.time += 0.00001;
		}
		uniqueKey.second->addKey( key );

		// make sure the right key is used for subsequent user interaction
		UniqueKey newUniqueKey( key, uniqueKey.second );
		tmp.insert( newUniqueKey );

		// update storage of the keys' values  when drag was initiated
		key = m_dragInitialKeys[uniqueKey];
		m_dragInitialKeys.erase( uniqueKey );
		// doing insertion last, guarantees Key is available for next lookup
		m_dragInitialKeys[newUniqueKey] = key;
	}

	m_selectedKeys = tmp;

	requestRender();
}

void AnimationGadget::frame() const
{
	Box3f b;

	// trying to frame to selected keys first
	if( !m_selectedKeys.empty() )
	{
		for( const auto &key : m_selectedKeys )
		{
			b.extendBy( V3f( key.first.time, key.first.value, 0 ) );
		}
	}
	// trying to frame to editable keys next
	else if( !m_curvePlugsEditable.empty() )
	{
		for( const auto &curvePlug : m_curvePlugsEditable )
		{
			const Animation::CurvePlug::Keys &keys = curvePlug->keys();

			for( const auto &key : keys )
 			{
				b.extendBy( V3f( key.time, key.value, 0 ) );
			}
		}
	}
	// trying to frame to visible keys next
	else if( !m_curvePlugsVisible.empty() )
	{
		for( const auto &curvePlug : m_curvePlugsVisible )
		{
			const Animation::CurvePlug::Keys &keys = curvePlug->keys();

			for( const auto &key : keys )
 			{
				b.extendBy( V3f( key.time, key.value, 0 ) );
			}
		}

	}
	// setting default framing as last resort
	// \todo: ideally this would frame to all curves that are part of the
	// animation node, but just not visible at the moment
	else
	{
		b = Box3f( V3f( -1, -1, 0), V3f( 10, 10, 0 ) );
	}

	Box3f bound( b.min - V3f( .1 ), b.max + V3f( .1 ) );  // \todo: only needed for single key framing. let's rethink that
	V3f center = bound.center();
	bound.min = center + ( bound.min - center ) * 1.2;
	bound.max = center + ( bound.max - center ) * 1.2;
	m_viewportGadget->frame( bound );

	return;
}

bool AnimationGadget::buttonPress( GadgetPtr gadget, const ButtonEvent &event )
{
	return true;
}

bool AnimationGadget::buttonRelease( GadgetPtr gadget, const ButtonEvent &event )
{

	if( event.button == ButtonEvent::Left )
	{
		m_selectedKeys.clear();
	}

	requestRender();

	return true;
}

IECore::RunTimeTypedPtr AnimationGadget::dragBegin( GadgetPtr gadget, const DragDropEvent &event )
{
	V3f i;
	if( !event.line.intersect( Plane3f( V3f( 0, 0, 1 ), 0 ), i ) )
	{
		return nullptr;
	}

	switch (event.buttons)
	{

	case ButtonEvent::Left :
	{
		m_dragMode = DragMode::Selecting;
		break;
	}

	case ButtonEvent::Middle :
	{

		// storing keys at beginning of drag
		// \todo: could form the basis of a hacky undo? I'd much rather use the
		// global undo system rather than listening for Ctrl-z and then
		// restoring what was stored in this container?
		m_dragInitialKeys.clear();
		for( auto &key : m_selectedKeys )
		{
			m_dragInitialKeys[key] = key.first;
		}

		bool shiftHeld = event.modifiers & DragDropEvent::Shift;
		if( shiftHeld )
		{
			m_moveAxis = MoveAxis::Undefined;
		}

		m_dragMode = DragMode::Moving;

		break;
	}

	default:
	{
		return nullptr;
	}

	}

	m_dragStartPosition = m_lastDragPosition = V2f( i.x, i.y );

	return IECore::NullObject::defaultNullObject();
}

bool AnimationGadget::dragEnter( GadgetPtr gadget, const DragDropEvent &event )
{
	V3f i;
	if( !event.line.intersect( Plane3f( V3f( 0, 0, 1 ), 0 ), i ) )
	{
		return false;
	}

	m_lastDragPosition = V2f( i.x, i.y );
	requestRender();
	return true;
}

bool AnimationGadget::dragMove( GadgetPtr gadget, const DragDropEvent &event )
{
	V3f i;
	if( !event.line.intersect( Plane3f( V3f( 0, 0, 1 ), 0 ), i ) )
	{
		return false;
	}

	if( m_dragMode == DragMode::Moving )
	{

		if( m_moveAxis == MoveAxis::Undefined )
		{
			if( abs( i.x - m_dragStartPosition.x ) >= abs ( i.y - m_dragStartPosition.y ) )
			{
				m_moveAxis = MoveAxis::X;
				Pointer::setCurrent( "moveHorizontally" );
			}
			else
			{
				m_moveAxis = MoveAxis::Y;
				Pointer::setCurrent( "moveVertically" );
			}
		}

		moveKeyframes( V2f( i.x, i.y ) - m_dragStartPosition );
	}

	m_lastDragPosition = V2f( i.x, i.y );

	requestRender();
	return true;
}

bool AnimationGadget::dragEnd( GadgetPtr gadget, const DragDropEvent &event )
{
	V3f i;
	if( !event.line.intersect( Plane3f( V3f( 0, 0, 1 ), 0 ), i ) )
	{
		return false;
	}

	switch (m_dragMode)
	{

	case DragMode::Selecting :
	{

		Box2f b;
		b.extendBy( V2f( m_dragStartPosition.x, m_dragStartPosition.y ) );
		b.extendBy( V2f( m_lastDragPosition.x, m_lastDragPosition.y ) );

		bool controlHeld = event.modifiers & ButtonEvent::Control;
		if( !controlHeld )
		{
			m_selectedKeys.clear();
		}

		for( auto &curvePlug : m_curvePlugsEditable )
		{
			const Animation::CurvePlug::Keys &keys = curvePlug->keys();
			for( Animation::CurvePlug::Keys::iterator it = keys.begin(); it != keys.end(); ++it )
			{
				const Animation::Key &key = (*it);

				if( b.intersects( V2f( key.time, key.value ) ) )
				{
					m_selectedKeys.emplace( key, curvePlug );
				}
			}
		}

		break;
	}
	case DragMode::Moving :
		break;

	default :
		break;

	}

	m_dragMode = DragMode::None;
	m_moveAxis = MoveAxis::Both;
	Pointer::setCurrent( "" );

	requestRender();

	return true;
}

bool AnimationGadget::keyPress( GadgetPtr gadget, const KeyEvent &event )
{
	if( event.key == "I" )
	{
		insertKeyframes();
		return true;
	}

	if( event.key == "F" )
	{
		frame();
		return true;
	}

	if( event.key == "Delete" )
	{
		removeKeyframes();
		return true;
	}

	return false;
}
AnimationGadget::CurveGadget::CurveGadget( std::string name, const Gaffer::Animation::CurvePlug *curvePlug )
	: m_name( name ), m_curvePlug( curvePlug )
{

	enterSignal().connect( boost::bind( &AnimationGadget::CurveGadget::enter, this, ::_1, ::_2 ) );
	leaveSignal().connect( boost::bind( &AnimationGadget::CurveGadget::leave, this, ::_1, ::_2 ) );

}

AnimationGadget::CurveGadget::~CurveGadget()
{
}

void AnimationGadget::CurveGadget::doRenderLayer( Layer layer, const Style *style ) const
{
	const ViewportGadget *viewportGadget = ancestor<ViewportGadget>();

	ViewportGadget::RasterScope rasterScope( viewportGadget );

	if( !viewportGadget )
	{
		return;
	}

	if( layer != AnimationLayer::Curves )
	{
		return;
	}

	// \todo This would benefit a bit from frustum culling, 
	//       Needs either computation of visible area in world space or access to the parent

	Animation::Key previousKey = Animation::Key();
	V2f previousKeyPosition = V2f( 0 );

	const Animation::CurvePlug::Keys &keys = m_curvePlug->keys();
	for( Animation::CurvePlug::Keys::const_iterator it = keys.begin(); it != keys.end(); ++it )
	{
		const Animation::Key &key = (*it);

		V2f keyPosition = viewportGadget->worldToRasterSpace( V3f( key.time, key.value, 0 ) );
		if( previousKey.type != Gaffer::Animation::Invalid )
		{
			// \todo: needs tangent computation/hand-off as soon as we support more interpolation modes
			//        consider passing interpolation into renderCurveSegment to handle all drawing there

			const Imath::Color3f *userColor = colorFromName();

			if( key.type == Gaffer::Animation::Linear )
			{
				style->renderCurveSegment( previousKeyPosition, keyPosition, /* inTangent */ V2f( 0 ), /* outTangent */ V2f( 0 ), getHighlighted() ? Style::HighlightedState : Style::NormalState, userColor );
			}
			else if( key.type == Gaffer::Animation::Step )
			{
				style->renderLine( IECore::LineSegment3f( V3f( previousKeyPosition.x, previousKeyPosition.y, 0 ), V3f( keyPosition.x, previousKeyPosition.y, 0) ), 0.5, userColor );
				style->renderLine( IECore::LineSegment3f( V3f( keyPosition.x, previousKeyPosition.y, 0 ), V3f( keyPosition.x, keyPosition.y, 0 ) ), 0.5, userColor );
			}
		}

		// style->renderKeyFrame( keyPosition, getHighlighted() ? Style::HighlightedState : Style::NormalState );

		previousKey = key;
		previousKeyPosition = keyPosition;
	}

}

std::string AnimationGadget::CurveGadget::getToolTip( const IECore::LineSegment3f &line ) const
{
	return m_name;
}

void AnimationGadget::CurveGadget::enter( GadgetPtr gadget, const ButtonEvent &event )
{
	setHighlighted( true );
}

void AnimationGadget::CurveGadget::leave( GadgetPtr gadget, const ButtonEvent &event )
{
	setHighlighted( false );
}

// \todo: consider making the colorForAxes function in StandardStyle public?
const Imath::Color3f *AnimationGadget::CurveGadget::colorFromName() const
{
	if( boost::ends_with( m_name.string(), ".x" ) )
	{
		return new Imath::Color3f( 0.73, 0.17, 0.17 );
	}

	if( boost::ends_with( m_name.string(), ".y" ) )
	{
		return new Imath::Color3f( 0.2, 0.57, 0.2 );
	}

	if( boost::ends_with( m_name.string(), ".z" ) )
	{
		return new Imath::Color3f( 0.2, 0.36, 0.74 );
	}

	return nullptr;
}
