//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2017, John Haddon. All rights reserved.
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

#include "GafferUI/RotateHandle.h"

#include "IECore/Exception.h"
#include "IECore/Export.h"

IECORE_PUSH_DEFAULT_VISIBILITY
#include "OpenEXR/ImathEuler.h"
#include "OpenEXR/ImathMatrixAlgo.h"
#include "OpenEXR/ImathSphere.h"
IECORE_POP_DEFAULT_VISIBILITY

#include "boost/bind.hpp"

using namespace Imath;
using namespace IECore;
using namespace GafferUI;

//////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////

namespace
{

float closestRotation( const V2f &p, float targetRotation )
{
	const float r = atan2( p.y, p.x );
	return Eulerf::angleMod( r - targetRotation ) + targetRotation;
}

} // namespace

//////////////////////////////////////////////////////////////////////////
// RotateHandle
//////////////////////////////////////////////////////////////////////////

IE_CORE_DEFINERUNTIMETYPED( RotateHandle );

RotateHandle::RotateHandle( Style::Axes axes )
	:	Handle( defaultName<RotateHandle>() ), m_axes( Style::X )
{
	setAxes( axes );
	dragMoveSignal().connect( boost::bind( &RotateHandle::dragMove, this, ::_2 ) );
	mouseMoveSignal().connect( boost::bind( &RotateHandle::mouseMove, this, ::_2 ) );
}

RotateHandle::~RotateHandle()
{
}

void RotateHandle::setAxes( Style::Axes axes )
{
	if( axes == m_axes )
	{
		return;
	}

	switch( axes )
	{
		case Style::X :
		case Style::Y :
		case Style::Z :
		case Style::XYZ :
			break;
		default :
			throw IECore::Exception( "Unsupported axes" );
	}

	m_axes = axes;
	requestRender();
}

Style::Axes RotateHandle::getAxes() const
{
	return m_axes;
}

Imath::V3i RotateHandle::axisMask() const
{
	switch( m_axes )
	{
		case Style::X :
			return V3i( 1, 0, 0 );
		case Style::Y :
			return V3i( 0, 1, 0 );
		case Style::Z :
			return V3i( 0, 0, 1 );
		case Style::XYZ :
			return V3i( 1, 1, 1 );
		default :
			// Checks in `setAxes()` prevent us getting here
			return V3i( 0 );
	}
}

Imath::Eulerf RotateHandle::rotation( const DragDropEvent &event ) const
{
	if( m_axes == Style::XYZ )
	{
		const LineSegment3f line = event.line * fullTransform() * m_dragBeginWorldTransform.inverse();
		const M44f m = rotationMatrix( m_dragBeginPointOnSphere, pointOnSphere( line ) );
		Eulerf e; e.extract( m );
		return e;
	}

	const float rotationFactor = transformationFactor();
	const float r = ( closestRotation( m_drag.position( event ), m_rotation ) - closestRotation( m_drag.startPosition(), 0.0f ) ) * rotationFactor;

	switch( m_axes )
	{
		case Style::X :
			return Eulerf( V3f( r, 0, 0 ) );
		case Style::Y :
			return Eulerf( V3f( 0, r, 0 ) );
		case Style::Z :
			return Eulerf( V3f( 0, 0, r ) );
		default :
			// Checks in `setAxes()` prevent us getting here
			return Eulerf();
	}
}

void RotateHandle::renderHandle( const Style *style, Style::State state ) const
{
	style->renderRotateHandle( m_axes, state, m_highlightVector );
}

void RotateHandle::dragBegin( const DragDropEvent &event )
{
	switch( m_axes )
	{
		case Style::X :
			m_drag = PlanarDrag( this, V3f( 0 ), V3f( 0, 1, 0 ), V3f( 0, 0, 1 ), event );
			m_rotation = closestRotation( m_drag.startPosition(), 0.0f );
			break;
		case Style::Y :
			m_drag = PlanarDrag( this, V3f( 0 ), V3f( 0, 0, 1 ), V3f( 1, 0, 0 ), event );
			m_rotation = closestRotation( m_drag.startPosition(), 0.0f );
			break;
		case Style::Z :
			m_drag = PlanarDrag( this, V3f( 0 ), V3f( 1, 0, 0 ), V3f( 0, 1, 0 ), event );
			m_rotation = closestRotation( m_drag.startPosition(), 0.0f );
			break;
		case Style::XYZ :
			m_dragBeginWorldTransform = fullTransform();
			m_dragBeginPointOnSphere = pointOnSphere( event.line );
			break;
		default :
			// Checks in `setAxes()` prevent us getting here
			break;
	}
}

bool RotateHandle::dragMove( const DragDropEvent &event )
{
	if( m_axes == Style::XYZ )
	{
		m_highlightVector = pointOnSphere( event.line );
		requestRender();
	}
	else
	{
		// We can only recover an angle in the range -PI, PI from the 2d position
		// that our drag gives us, but we want to be able to support continuous
		// values and multiple revolutions. Here we keep track of the current rotation
		// position so that we can adjust correctly in `RotateHandle::rotation()`.
		m_rotation = closestRotation( m_drag.position( event ), m_rotation );
	}
	return false;
}

bool RotateHandle::mouseMove( const ButtonEvent &event )
{
	m_highlightVector = pointOnSphere( event.line );
	requestRender();
	return true;
}

Imath::V3f RotateHandle::pointOnSphere( const IECore::LineSegment3f &line ) const
{
	const LineSegment3f scaledLine = line * M44f().scale( V3f( 1 ) / rasterScaleFactor() );
	const Imath::Sphere3f sphere( V3f( 0 ), 1.0f );
	V3f result;
	if( !sphere.intersect( Line3f( scaledLine.p0, scaledLine.p1 ), result ) )
	{
		result = scaledLine.closestPointTo( V3f( 0 ) );
		result.normalize();
	}
	return result;
}
