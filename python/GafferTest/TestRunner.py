##########################################################################
#
#  Copyright (c) 2019, Image Engine Design Inc. All rights reserved.
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

import unittest
import functools
import json
import time
import collections

import Gaffer

# TestRunner capable of measuring performance of certain
# tests and failing them if they contain regressions compared
# to previous results.
class TestRunner( unittest.TextTestRunner ) :

	def __init__( self, previousResultsFile = "" ) :

		unittest.TextTestRunner.__init__(
			self,
			verbosity = 2,
		)

		self.__previousResultsFile = previousResultsFile

	# Decorator used to annotate tests which measure performance.
	class PerformanceTestMethod( object ) :

		def __init__( self, repeat = 3, acceptableDifference = 0.01 ) :

			self.__repeat = repeat
			self.__acceptableDifference = acceptableDifference

		# Called to return the decorated method.
		def __call__( self, method ) :

			@functools.wraps( method )
			def wrapper( *args, **kw ) :

				timings = []
				for i in range( 0, self.__repeat ) :
					Gaffer.ValuePlug.clearCache() # Put each iteration on an equal footing
					TestRunner.PerformanceScope._total = None
					t = time.time()
					result = method( *args, **kw )
					totalTime = time.time() - t
					scopedTime = TestRunner.PerformanceScope._total
					timings.append( scopedTime if scopedTime is not None else totalTime )

				# Stash timings so they can be recovered
				# by TestRunner.__Result.
				args[0].timings = timings

				# If previous timings are available, then
				# compare against them and throw if a regression
				# is detected.
				previousTimings = getattr( args[0], "previousTimings" )
				if previousTimings :
					args[0].assertLessEqual( min( timings ), min( previousTimings ) + self.__acceptableDifference )

				return result

			wrapper.performanceTestMethod = True

			return wrapper

	# Context manager used to time only specific blocks
	# within a PerformanceTestMethod.
	class PerformanceScope( object ) :

		# Protected to allow access by PerformanceTestMethod.
		_total = None

		def __enter__( self ) :

			self.__startTime = time.time()

		def __exit__( self, type, value, traceBack ) :

			t = time.time() - self.__startTime
			if TestRunner.PerformanceScope._total is not None :
				TestRunner.PerformanceScope._total += t
			else :
				TestRunner.PerformanceScope._total = t

	def run( self, test ) :

		result = unittest.TextTestRunner.run( self, test )
		result.writePerformance()
		return result

	# Adds a skip decorator to all non-performance-related tests.
	@staticmethod
	def filterPerformanceTests( test ) :

		if isinstance( test, unittest.TestSuite ) :
			for t in test :
				TestRunner.filterPerformanceTests( t )
		elif isinstance( test, unittest.TestCase ) :
			testMethod = getattr( test, test._testMethodName )
			if not getattr( testMethod, "performanceTestMethod", False  ) :
				setattr(
					test, test._testMethodName,
					unittest.skip( "Not a performance test" )( testMethod )
				)

	def _makeResult( self ) :

		return self.__Result(
			self.stream, self.descriptions, self.verbosity,
			previousResultsFile = self.__previousResultsFile
		)

	class __Result( unittest.TextTestResult ) :

		def __init__( self, stream, descriptions, verbosity, previousResultsFile ) :

			unittest.TextTestResult.__init__( self, stream, descriptions, verbosity )

			self.__results = collections.OrderedDict()

			if previousResultsFile :
				with open( previousResultsFile ) as f :
					self.__previousResults = json.load( f )
			else :
				self.__previousResults = {}

			self.__performanceImprovements = []

		# Methods unique to __Result

		def save( self, fileName ) :

			with open( fileName, "w" ) as f :
				json.dump( self.__results, f, indent = 4 )

		def writePerformance( self ) :

			if not len( self.__performanceImprovements ) :
				return

			self.stream.write( "{0}\n".format( self.separator2 ) )

			self.stream.write( "{n} Performance Improvement{s} :\n\n".format(
				n = len( self.__performanceImprovements ),
				s = "s" if len( self.__performanceImprovements ) > 1 else ""
			) )
			for s in self.__performanceImprovements :
				self.stream.write( "{}\n".format( s ) )

		# Overrides for TextTestResult methods

		def startTest( self, test ) :

			previousResults = self.__previousResults.get( str( test ), {} )
			test.previousTimings = previousResults.get( "timings", [] )

			unittest.TextTestResult.startTest( self, test )

		def addSuccess( self, test ) :

			unittest.TextTestResult.addSuccess( self, test )

			timings = getattr( test, "timings", None )
			if timings and test.previousTimings :
				new = min( timings )
				old = min( test.previousTimings )
				reduction = 100 * (old-new)/old
				if reduction > 2 :
					self.__performanceImprovements.append(
						"- {test} : was {old:.2f}s now {new:.2f}s ({reduction:.0f}% reduction)".format(
							test = str( test), old = old, new = new, reduction = reduction
						)
					)

			self.__addResult( test, "success" )

		def addError( self, test, error ) :

			unittest.TextTestResult.addError( self, test, error )
			self.__addResult( test, "error" )

		def addFailure( self, test, error ) :

			unittest.TextTestResult.addFailure( self, test, error )
			self.__addResult( test, "failure" )

		def wasSuccessful( self ) :

			return unittest.TextTestResult.wasSuccessful( self )

		# Private methods

		def __addResult( self, test, result ) :

			d = {
				"result" : result
			}

			timings = getattr( test, "timings", None )
			if timings :
				d["timings"] = timings

			self.__results[str(test)] = d
