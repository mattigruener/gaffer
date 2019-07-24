##########################################################################
#
#  Copyright (c) 2012, John Haddon. All rights reserved.
#  Copyright (c) 2013, Image Engine Design Inc. All rights reserved.
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

import os
import unittest
import imath

import IECore
import IECoreScene

import Gaffer
import GafferTest
import GafferScene
import GafferSceneTest

class SceneTestCase( GafferTest.TestCase ) :

	def setUp( self ) :

		GafferTest.TestCase.setUp( self )

		sanitiser = GafferSceneTest.ContextSanitiser()
		sanitiser.__enter__()
		self.addCleanup( sanitiser.__exit__, None, None, None )

	def assertSceneValid( self, scenePlug, assertBuiltInSetsComplete=True ) :

		def walkScene( scenePath ) :

			# at least pull on the attributes, even though we don't have any test cases for that right now
			attributes = scenePlug.attributes( scenePath )

			thisBound = scenePlug.bound( scenePath )

			o = scenePlug.object( scenePath, _copy = False )
			if isinstance( o, IECoreScene.VisibleRenderable ) :
				 if not IECore.BoxAlgo.contains( thisBound, o.bound() ) :
					self.fail( "Bound %s does not contain object %s at %s" % ( thisBound, o.bound(), scenePath ) )
			if isinstance( o, IECoreScene.Primitive ) :
				if "P" in o :
					if not isinstance( o["P"].data, IECore.V3fVectorData ) :
						self.fail( "Object %s has incorrect type %s for primitive variable \"P\"" % ( scenePath, o["P"].data.typeName() ) )
					if o["P"].data.getInterpretation() != IECore.GeometricData.Interpretation.Point :
						self.fail( "Object %s has primitive variable \"P\" with incorrect interpretation" % scenePath )
				if not o.arePrimitiveVariablesValid() :
					self.fail( "Object %s has invalid primitive variables" % scenePath )


			unionOfTransformedChildBounds = imath.Box3f()
			childNames = scenePlug.childNames( scenePath, _copy = False )
			for childName in childNames :

				childPath = IECore.InternedStringVectorData( scenePath )
				childPath.append( childName )

				childBound = scenePlug.bound( childPath )
				childTransform = scenePlug.transform( childPath )
				childBound = childBound * childTransform

				unionOfTransformedChildBounds.extendBy( childBound )

				walkScene( childPath )

			if not IECore.BoxAlgo.contains( thisBound, unionOfTransformedChildBounds ) :
				self.fail( "Bound ( %s ) does not contain children ( %s ) at %s" % ( thisBound, unionOfTransformedChildBounds, scenePath ) )

		# check that the root doesn't have any properties it shouldn't
		self.assertEqual( scenePlug.attributes( "/" ), IECore.CompoundObject() )
		self.assertEqual( scenePlug.transform( "/" ), imath.M44f() )
		self.assertEqual( scenePlug.object( "/" ), IECore.NullObject() )

		# then walk the scene to check the bounds
		walkScene( IECore.InternedStringVectorData() )

		self.assertSetsValid( scenePlug )

		if assertBuiltInSetsComplete :
			self.assertBuiltInSetsComplete( scenePlug )

	def assertPathExists( self, scenePlug, path ) :

		if isinstance( path, str ) :
			path = path.strip( "/" ).split( "/" )

		for i in range( 0, len( path ) ) :
			self.assertTrue(
				path[i] in scenePlug.childNames( "/" + "/".join( path[:i] ) ),
				"\"{childName}\" in {scene}.childNames( \"{location}\" )".format(
					childName = path[i],
					scene = scenePlug.relativeName( scenePlug.ancestor( Gaffer.ScriptNode ) ),
					location =  "/" + "/".join( path[:i] )
				)
			)

	## Checks that all paths referenced by sets do exist.
	def assertSetsValid( self, scenePlug ) :

		for setName in scenePlug["setNames"].getValue() :
			s = scenePlug.set( setName )
			for path in s.value.paths() :
				self.assertPathExists( scenePlug, path )

	## Checks that all lights, coordinate systems and cameras
	# in the scene are in the appropriate built-in sets.
	def assertBuiltInSetsComplete( self, scenePlug ) :

		setNames = scenePlug["setNames"].getValue()
		lightSet = scenePlug.set( "__lights" ) if "__lights" in setNames else IECore.PathMatcherData()
		cameraSet = scenePlug.set( "__cameras" ) if "__cameras" in setNames else IECore.PathMatcherData()
		coordinateSystemSet = scenePlug.set( "__coordinateSystems" ) if "__coordinateSystems" in setNames else IECore.PathMatcherData()

		def walkScene( scenePath ) :

			object = scenePlug.object( scenePath, _copy = False )
			if isinstance( object, IECoreScene.Camera ) :
				self.assertTrue(
					cameraSet.value.match( scenePath ) & IECore.PathMatcher.Result.ExactMatch,
					scenePath + " in __cameras set"
				)
			elif isinstance( object, IECoreScene.CoordinateSystem ) :
				self.assertTrue(
					coordinateSystemSet.value.match( scenePath ) & IECore.PathMatcher.Result.ExactMatch,
					scenePath + " in __coordinateSystems set"
				 )

			attributes = scenePlug.attributes( scenePath, _copy = False )
			if any( [ n == "light" or n.endswith( ":light" ) for n in attributes.keys() ] ) :
				self.assertTrue(
					lightSet.value.match( scenePath ) & IECore.PathMatcher.Result.ExactMatch,
					scenePath + " in __lights set"
				 )

			childNames = scenePlug.childNames( scenePath, _copy = False )
			for childName in childNames :
				walkScene( os.path.join( scenePath, str( childName ) ) )

		walkScene( "/" )

	allPathChecks = { "bound", "transform", "attributes", "object", "childNames" }
	allSceneChecks = allPathChecks | { "sets", "globals" }

	def assertPathsEqual( self, scenePlug1, scenePath1, scenePlug2, scenePath2, checks = allPathChecks ) :

		assert( checks.issubset( self.allPathChecks ) )

		for childPlugName in checks :
			value1 = getattr( scenePlug1, childPlugName )( scenePath1 )
			value2 = getattr( scenePlug2, childPlugName )( scenePath2 )
			self.assertEqual(
				value1, value2,
				"{0} != {1} : comparing {childPlugName} at {paths}".format(
					unittest.util.safe_repr( value1 ), unittest.util.safe_repr( value2 ),
					childPlugName = childPlugName, paths = self.__formatPaths( scenePath1, scenePath2 )
				)
			)

	def assertScenesEqual( self, scenePlug1, scenePlug2, scenePlug2PathPrefix = "", pathsToIgnore = (), checks = allSceneChecks ) :

		assert( checks.issubset( self.allSceneChecks ) )

		def walkScene( scenePath1, scenePath2 ) :

			if ( not pathsToIgnore ) or ( self.__pathToString( scenePath1 ) not in pathsToIgnore ) :
				self.assertPathsEqual( scenePlug1, scenePath1, scenePlug2, scenePath2, checks.intersection( self.allPathChecks ) )

			childNames = scenePlug1.childNames( scenePath1 )
			for childName in childNames :

				childPath1 = IECore.InternedStringVectorData( scenePath1 )
				childPath1.append( childName )

				childPath2 = IECore.InternedStringVectorData( scenePath2 )
				childPath2.append( childName )

				walkScene( childPath1, childPath2 )

		scenePath1 = IECore.InternedStringVectorData()
		scenePath2 = IECore.InternedStringVectorData()
		if scenePlug2PathPrefix :
			scenePath2.extend( IECore.InternedStringVectorData( scenePlug2PathPrefix[1:].split( "/" ) ) )

		walkScene( scenePath1, scenePath2 )

		if "globals" in checks :
			self.assertEqual( scenePlug1.globals(), scenePlug2.globals() )
		if "sets" in checks :
			self.assertEqual( scenePlug1.setNames(), scenePlug2.setNames() )
			for setName in scenePlug1.setNames() :
				self.assertEqual( scenePlug1.set( setName ), scenePlug2.set( setName ) )

	def assertPathHashesEqual( self, scenePlug1, scenePath1, scenePlug2, scenePath2, checks = allPathChecks ) :

		assert( checks.issubset( self.allPathChecks ) )

		for childPlugName in checks :
			hash1 = getattr( scenePlug1, childPlugName + "Hash" )( scenePath1 )
			hash2 = getattr( scenePlug2, childPlugName + "Hash" )( scenePath2 )
			self.assertEqual(
				hash1, hash2,
				"{0} != {1} : comparing {childPlugName}Hash at {paths}".format(
					unittest.util.safe_repr( hash1 ), unittest.util.safe_repr( hash2 ),
					childPlugName = childPlugName, paths = self.__formatPaths( scenePath1, scenePath2 )
				)
			)

	def assertPathHashesNotEqual( self, scenePlug1, scenePath1, scenePlug2, scenePath2, checks = allPathChecks ) :

		assert( checks.issubset( self.allPathChecks ) )

		for childPlugName in checks :
			hash1 = getattr( scenePlug1, childPlugName + "Hash" )( scenePath1 )
			hash2 = getattr( scenePlug2, childPlugName + "Hash" )( scenePath2 )
			self.assertNotEqual(
				hash1, hash2,
				"{0} == {1} : comparing {childPlugName}Hash at {paths}".format(
					unittest.util.safe_repr( hash1 ), unittest.util.safe_repr( hash2 ),
					childPlugName = childPlugName, paths = self.__formatPaths( scenePath1, scenePath2 )
				)
			)

	def assertSceneHashesEqual( self, scenePlug1, scenePlug2, scenePlug2PathPrefix = "", pathsToIgnore = (), checks = allSceneChecks ) :

		assert( checks.issubset( self.allSceneChecks ) )

		def walkScene( scenePath1, scenePath2 ) :

			if ( not pathsToIgnore ) or ( self.__pathToString( scenePath1 ) not in pathsToIgnore ) :

				self.assertPathHashesEqual( scenePlug1, scenePath1, scenePlug2, scenePath2, checks.intersection( self.allPathChecks ) )

			childNames = scenePlug1.childNames( scenePath1 )
			for childName in childNames :

				childPath1 = IECore.InternedStringVectorData( scenePath1 )
				childPath1.append( childName )

				childPath2 = IECore.InternedStringVectorData( scenePath2 )
				childPath2.append( childName )

				walkScene( childPath1, childPath2 )

		scenePath1 = IECore.InternedStringVectorData()
		scenePath2 = IECore.InternedStringVectorData()
		if scenePlug2PathPrefix :
			scenePath2.extend( IECore.InternedStringVectorData( scenePlug2PathPrefix[1:].split( "/" ) ) )

		walkScene( scenePath1, scenePath2 )

		if "globals" in checks :
			self.assertEqual( scenePlug1.globalsHash(), scenePlug2.globalsHash() )
		if "sets" in checks :
			self.assertEqual( scenePlug1.setNamesHash(), scenePlug2.setNamesHash() )
			for setName in scenePlug1.setNames() :
				self.assertEqual( scenePlug1.setHash( setName ), scenePlug2.setHash( setName ) )

	def assertSceneHashesNotEqual( self, scenePlug1, scenePlug2, scenePlug2PathPrefix = "", pathsToIgnore = (), checks = allSceneChecks ) :

		def walkScene( scenePath1, scenePath2 ) :

			if ( not pathsToIgnore ) or ( self.__pathToString( scenePath1 ) not in pathsToIgnore ) :

				pathChecks = checks.intersection( self.allPathChecks )
				if len( scenePath1 ) == 0 :
					# Hashes will automatically be equal for these plugs at the root,
					# because they are dealt with automatically in the SceneNode base class.
					pathChecks -= { "attributes", "object", "transform" }

				self.assertPathHashesNotEqual( scenePlug1, scenePath1, scenePlug2, scenePath2, pathChecks )

			childNames = scenePlug1.childNames( scenePath1 )
			for childName in childNames :

				childPath1 = IECore.InternedStringVectorData( scenePath1 )
				childPath1.append( childName )

				childPath2 = IECore.InternedStringVectorData( scenePath2 )
				childPath2.append( childName )

				walkScene( childPath1, childPath2 )

		scenePath1 = IECore.InternedStringVectorData()
		scenePath2 = IECore.InternedStringVectorData()
		if scenePlug2PathPrefix :
			scenePath2.extend( IECore.InternedStringVectorData( scenePlug2PathPrefix[1:].split( "/" ) ) )

		walkScene( scenePath1, scenePath2 )

		if "globals" in checks :
			self.assertNotEqual( scenePlug1.globalsHash(), scenePlug2.globalsHash() )
		if "sets" in checks :
			self.assertNotEqual( scenePlug1.setNamesHash(), scenePlug2.setNamesHash() )
			for setName in scenePlug1.setNames() :
				self.assertNotEqual( scenePlug1.setHash( setName ), scenePlug2.setHash( setName ) )

	def assertBoxesEqual( self, box1, box2 ) :

		for n in "min", "max" :
			v1 = getattr( box1, n )
			v2 = getattr( box1, n )
			for i in range( 0, 3 ) :
				self.assertEqual( v1[i], v2[i] )

	def assertBoxesAlmostEqual( self, box1, box2, places ) :

		for n in "min", "max" :
			v1 = getattr( box1, n )()
			v2 = getattr( box1, n )()
			for i in range( 0, 3 ) :
				self.assertAlmostEqual( v1[i], v2[i], places )

	__uniqueInts = {}
	@classmethod
	def uniqueInt( cls, key ) :

		value = cls.__uniqueInts.get( key, 0 )
		value += 1
		cls.__uniqueInts[key] = value

		return value

	def __pathToString( self, path ) :

		return "/" + "/".join( [ p.value() for p in path ] )

	def __formatPaths( self, path1, path2 ) :

		if not isinstance( path1, basestring ) :
			path1 = self.__pathToString( path1 )

		if not isinstance( path2, basestring ) :
			path2 = self.__pathToString( path2 )

		if path1 == path2 :
			return "\"{0}\"".format( path1 )
		else :
			return "\"{0}\" and \"{1}\"".format( path1, path2 )
