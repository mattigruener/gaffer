##########################################################################
#
#  Copyright (c) 2015, Image Engine Design Inc. All rights reserved.
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

import Gaffer
import GafferUI
import GafferScene

Gaffer.Metadata.registerNode(

	GafferScene.Seeds,

	"description",
	"""
	Scatters points evenly over the surface of meshes.
	This can be particularly useful in conjunction with
	the Instancer, which can then apply instances to
	each point.
	""",

	plugs = {

		"parent" : [

			"description",
			"""
			The location of the mesh to scatter the
			points over. The generated points will
			be parented under this location. This is
			ignored when a filter is connected, in
			which case the filter may specify multiple
			locations containing meshes to scatter points
			over.
			""",

		],

		"name" : [

			"description",
			"""
			The name given to the object generated -
			this will be placed under the parent in
			the scene hierarchy.
			""",

		],

		"density" : [

			"description",
			"""
			The number of points per unit area of the mesh,
			measured in object space.
			""",

		],

		"densityPrimitiveVariable" : [

			"description",
			"""
			A float primitive variable used to specify a varying
			point density across the surface of the mesh. Multiplied
			with the density setting above.
			""",

		],

		"pointType" : [

			"description",
			"""
			The render type of the points. This defaults to
			"gl:point" so that the points are rendered in a
			lightweight manner in the viewport.
			""",

			"preset:GL Point", "gl:point",
			"preset:Particle", "particle",
			"preset:Sphere", "sphere",
			"preset:Disk", "disk",
			"preset:Patch", "patch",
			"preset:Blobby", "blobby",

			"plugValueWidget:type", "GafferUI.PresetsPlugValueWidget",

		]

	}

)
