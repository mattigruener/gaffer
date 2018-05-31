//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2018, Matti Gruner. All rights reserved.
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

// \todo: Needs a way to determine fps at some point. For now, we always use 24 fps
template<typename T>
float frameToTime( T frame )
{
	return frame / 24.0;
}

template<typename T>
float timeToFrame( T time )
{
	return time * 24.0;
}

class CurveGadget : public Gadget
{

	public :

		CurveGadget( std::string name, const Gaffer::Animation::CurvePlug *curvePlug );
		~CurveGadget() override;

		std::string getToolTip( const IECore::LineSegment3f &line ) const override;

	protected :

		void doRenderLayer( Layer layer, const Style *style ) const override;

	private :

		void enter( GadgetPtr gadget, const ButtonEvent &event );
		void leave( GadgetPtr gadget, const ButtonEvent &event );

		const Imath::Color3f *colorFromName() const;

		IECore::InternedString m_name;
		const Gaffer::Animation::CurvePlug *m_curvePlug;
};

IE_CORE_DECLAREPTR( CurveGadget );

} // namespace

//////////////////////////////////////////////////////////////////////////
// AnimationGadget implementation
//////////////////////////////////////////////////////////////////////////

IE_CORE_DEFINERUNTIMETYPED( AnimationGadget );

AnimationGadget::AnimationGadget()
	: m_viewportGadget( nullptr ), m_currentFrame( 0 ), m_dragStartPosition( 0 ), m_lastDragPosition( 0 ), m_dragMode( DragMode::None ), m_moveAxis( MoveAxis::Both ), m_snappingClosestKey( nullptr ), m_snappingClosestKeyTime( 0 )
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

	Imath::V2i resolution = m_viewportGadget->getViewport();

	ViewportGadget::RasterScope rasterScope( m_viewportGadget );

	switch ( layer )
	{

	case AnimationLayer::Grid :
	{
		// \todo: make this is a struct so that we have more descriptive access to members below?
		AxisDefinition xAxis, yAxis;
		computeGrid( xAxis, yAxis );

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
		// The CurveGadget does all the work for us
		break;
	}

	case AnimationLayer::Keys :
	{
		for( const Animation::CurvePlugPtr &curvePlug : m_curvePlugsEditable )
		{
			for( Animation::Key &key : *curvePlug )
			{
				bool isSelected = m_selectedKeys.count( Animation::KeyPtr( &key ) ) > 0;
				V2f keyPosition = m_viewportGadget->worldToRasterSpace( V3f( key.getTime(), key.getValue(), 0 ) );
				style->renderKeyFrame( keyPosition, isSelected ? Style::HighlightedState : Style::NormalState );
			}
		}
		break;
	}


	case AnimationLayer::Axes :
	{
		AxisDefinition xAxis, yAxis;
		computeGrid( xAxis, yAxis );

		// draw frame indication
		int currentFrameRasterPosition = m_viewportGadget->worldToRasterSpace( V3f( m_currentFrame / 24.0, 0, 0 ) ).x;
		style->renderLine( IECore::LineSegment3f( V3f( currentFrameRasterPosition, 0, 0 ), V3f( currentFrameRasterPosition, resolution.y, 0 ) ), 2.0, new Imath::Color3f( 1.0, 0.2, 0.2 ) );

		// drawing axes on top of everything
		// \todo: store colors and sizes somewhere a bit more global
		glColor4f( 38.0 / 255, 38.0 / 255, 38.0 / 255, 1.0 );
		style->renderSolidRectangle( Box2f( V2f( 0 ) , V2f( 60, resolution.y - 20 ) ) );
		style->renderSolidRectangle( Box2f( V2f( 0, resolution.y - 20 ) , V2f( resolution.x, resolution.y ) ) );

		boost::format formatX( "%.2f" );
		boost::format formatY( "%.3f" );
		int textScale = 10;

		// \todo Clean up code duplication.
		// \todo There's some potential here to reduce pushing/popping of matrices by pulling those out of the loop

		for( const auto &x : xAxis.main )
		{
			if( x.first < 60 )  // \todo: remove magic number
			{
				continue;
			}

			glPushMatrix();

			std::string label = boost::str( formatX % x.second );

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

			std::string label = boost::str( formatY % y.second );

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
		break;
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

// Compute grid line locations. Note that positions are given in raster space so
// that lines can get drawn directly.
// For the time-dimension we limit the computed locations to multiples of one
// frame plus one level of unlabeled, dividing lines. Resulting at a minimum
// distance between lines of a fifth of a frame when zoomed in all the way.
// For the value dimension we allow sub-steps as small as 0.001.
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
	float yStride = 1;

	// \todo the box's size() is unrealiable because it considers the box empty for the inverted coords we seem to have here
	V2f pxPerUnit = V2f(
		resolution.x / abs( viewportBoundsFrames.min.x - viewportBoundsFrames.max.x ),
		resolution.y / abs( viewportBounds.min.y - viewportBounds.max.y ) );

	// Compute the stride to use for the time dimension.
	if( pxPerUnit.x < labelMinSize.x )
	{
		xStride = 5;
		pxPerUnit.x *= 5;

		// If there's not enough space for this zoom level, try using every 10th frame.
 		while( pxPerUnit.x < labelMinSize.x && pxPerUnit.x != 0 )
		{
			xStride *= 10;
			pxPerUnit.x *= 10;
		}
	}

	// Compute the stride to use for the value dimension.
	if( pxPerUnit.y < labelMinSize.y )
	{
		yStride = 5;
		pxPerUnit.y *= 5;

		// If there's not enough space for this zoom level, increase the spacing
		// between values to be drawn.
		while( pxPerUnit.y < labelMinSize.y && pxPerUnit.y != 0 )
		{
			yStride *= 10;
			pxPerUnit.y *= 10;
		}
	}
	else
	{
		// If we actually have too much space between values, progressively
		// decrease the stride to show smaller value deltas.
		float scale = 1;
		while( pxPerUnit.y / 10.0 > labelMinSize.y && scale > 0.001 )
		{
			yStride *= 0.1;
			pxPerUnit /= 10.0;
			scale /= 10.0;
		}
	}

	// Compute line locations based on bounds and strides in both dimensions.
	int lowerBoundX = std::floor( viewportBoundsFrames.min.x / xStride ) * xStride - xStride;
	int upperBoundX = std::ceil( viewportBoundsFrames.max.x );
	for( int i = lowerBoundX; i < upperBoundX; i += xStride )
	{
		float time = frameToTime( i );
		x.main.push_back( std::make_pair( m_viewportGadget->worldToRasterSpace( V3f( time, 0, 0 ) ).x, i ) );

		float subStride = frameToTime( xStride / 5.0 );
		for( int s = 1; s < 5; ++s )
		{
			x.secondary.push_back( m_viewportGadget->worldToRasterSpace( V3f( time + s * subStride, 0, 0 ) ).x );
		}
	}

	float lowerBoundY = std::floor( viewportBounds.max.y / yStride ) * yStride - yStride;
	float upperBoundY = viewportBounds.min.y + yStride;
	for( float j = lowerBoundY; j < upperBoundY; j += yStride )
	{
			y.main.push_back( std::make_pair( m_viewportGadget->worldToRasterSpace( V3f( 0, j, 0 ) ).y, j ) );
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
	m_animationCurves.clear();

	for( const auto &plug : plugs )
	{
		if( Gaffer::Animation::CurvePlug *curvePlug = IECore::runTimeCast<Gaffer::Animation::CurvePlug>( plug->getInput()->parent() ) )
		{
			std::string plugName = plug->fullName();
			CurveGadget *curveGadget = new CurveGadget( plugName, curvePlug );
			addChild( curveGadget );
			m_animationCurves.push_back( curveGadget ); // todo: store in map to update when plug changes?

			m_curvePlugsVisible.push_back( curvePlug );

			if( Node *node = curvePlug->node() )
			{
				node->plugDirtiedSignal().connect( boost::bind( &AnimationGadget::plugDirtied, this, ::_1 ) );
			}
		}
	}

	requestRender();
}

void AnimationGadget::plugDirtied( Gaffer::Plug *plug )
{
	requestRender();
}

void AnimationGadget::setEditablePlugs( const std::vector<Gaffer::Plug *> &plugs )
{
	m_curvePlugsEditable.clear();
	for( auto &plug : plugs )
	{
		if( Gaffer::Animation::CurvePlug *curvePlug = IECore::runTimeCast<Gaffer::Animation::CurvePlug>( plug->getInput()->parent() ) )
		{
			m_curvePlugsEditable.push_back( curvePlug );
		}
	}

	// Update selection to only contain Keys that belong to editable CurvePlugs.
	for( auto &key : m_selectedKeys )
	{
		if( std::find( m_curvePlugsEditable.begin(), m_curvePlugsEditable.end(), key->parent() ) == m_curvePlugsEditable.end() )
		{
			m_selectedKeys.erase( key );
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
		if( !curvePlug->closestKey( time, 0.004 ) ) // \todo: use proper ticks
		{
			float value = curvePlug->evaluate( time );
			curvePlug->addKey( new Animation::Key( time, value ) ); // \todo: should eventually specify key type
		}
	}
	requestRender();
}

void AnimationGadget::removeKeyframes()
{
	for( const auto &keyPtr : m_selectedKeys )
	{
		Animation::CurvePlug *parent = keyPtr->parent();
		if( parent )
		{
			parent->removeKey( keyPtr );
		}
	}

	m_selectedKeys.clear();

	requestRender();
}

void AnimationGadget::moveKeyframes( const V2f currentDragPosition )
{
	// Compute snapping offset used for all keys
	float xSnappingCurrentOffset = 0;
	if( m_moveAxis != MoveAxis::Y )
	{
		float dragOffset = currentDragPosition.x - m_dragStartPosition.x;
		xSnappingCurrentOffset = dragOffset;
		if( m_snappingClosestKey )
		{
			float unsnappedFrame = timeToFrame( m_snappingClosestKeyTime + xSnappingCurrentOffset );
			xSnappingCurrentOffset = frameToTime( round( unsnappedFrame ) ) - m_snappingClosestKeyTime;
		}
	}

	for( auto &key : m_selectedKeys )
	{
		// Apply offset for key's value
		if( m_moveAxis != MoveAxis::X )
		{
			key->setValue( key->getValue() + currentDragPosition.y - m_lastDragPosition.y );
		}

		// Apply offset for key's time
		if( m_moveAxis != MoveAxis::Y )
		{
			float dragStartPosition = key->getTime() - m_xSnappingPreviousOffset;
			float newTime = dragStartPosition + xSnappingCurrentOffset;

			// If a key already exists on the new frame, we overwrite it, but
			// store it for reinserting should the drag continue and the frame
			// free up again.
			Animation::KeyPtr clashingKey = key->parent()->closestKey( newTime, 0.004 );
			if( clashingKey && clashingKey != key )
			{
				m_overwrittenKeys.emplace( clashingKey, clashingKey->parent() );
				clashingKey->parent()->removeKey( clashingKey );
			}

			key->setTime( newTime );
		}
	}

	// Check if any of the previously overwritten keys can be inserted back into the curve
	for( auto keyAndParent : m_overwrittenKeys )
	{
		Animation::KeyPtr clashingKey = keyAndParent.second->closestKey( keyAndParent.first->getTime(), 0.004 ); // \todo: use proper ticks

		if( clashingKey )
		{
			// frame is still occupied by another key.
			continue;
		}

		keyAndParent.second->addKey( keyAndParent.first );
		m_overwrittenKeys.erase( keyAndParent );
	}

	if( m_moveAxis != MoveAxis::Y )
	{
		m_xSnappingPreviousOffset = xSnappingCurrentOffset;
	}

	requestRender();
}

void AnimationGadget::frame() const
{
	Box3f b;

	if( !m_selectedKeys.empty() ) // trying to frame to selected keys first
	{
		for( const auto &key : m_selectedKeys )
		{
			b.extendBy( V3f( key->getTime(), key->getValue(), 0 ) );
		}
	}
	else if( !m_curvePlugsEditable.empty() ) // trying to frame to editable curves next
	{
		for( const auto &curvePlug : m_curvePlugsEditable )
		{
			for( const auto &key : *curvePlug )
 			{
				b.extendBy( V3f( key.getTime(), key.getValue(), 0 ) );
			}
		}
	}
	else if( !m_curvePlugsVisible.empty() ) // trying to frame to visible curves next
	{
		for( const auto &curvePlug : m_curvePlugsVisible )
		{
			for( const auto &key : *curvePlug )
 			{
				b.extendBy( V3f( key.getTime(), key.getValue(), 0 ) );
			}
		}
	}
	else // setting default framing as last resort
	{
		b = Box3f( V3f( -1, -1, 0), V3f( 1, 1, 0 ) );
	}

	// add some margins. necessary if only a single key was used for framing.
	Box3f bound( b.min - V3f( .1 ), b.max + V3f( .1 ) );
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

	switch ( event.buttons )
	{

	case ButtonEvent::Left :
	{
		m_dragMode = DragMode::Selecting;
		break;
	}

	case ButtonEvent::Middle :
	{
		// determine axis in which to move the keys in dragMove()
		bool shiftHeld = event.modifiers & DragDropEvent::Shift;
		if( shiftHeld )
		{
			m_moveAxis = MoveAxis::Undefined;
		}

		// determine closest key for snapping in dragMove()
		m_snappingClosestKey = nullptr;
		m_snappingClosestKeyTime = 0;
		m_xSnappingPreviousOffset = 0;

		m_dragMode = DragMode::Moving;

		// Clean up selection so that we operate on valid Keys only
		for( auto &key : m_selectedKeys )
		{
			if( !key->parent() )
			{
				m_selectedKeys.erase( key );
				continue;
			}
		}

		break;
	}

	default:
	{
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

	if( m_dragMode == DragMode::Moving && !m_selectedKeys.empty() )
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

		if( m_moveAxis != MoveAxis::Y && !m_snappingClosestKey )
		{
			// determine position of selected keyframe that is closest to pointer
			// \todo: move into separate function, ideally consolidate with Animation::CurvePlug::closestKey?
			auto rightIt = m_selectedKeys.lower_bound( Animation::KeyPtr( new Animation::Key(i.x, 0) ) );

			if( rightIt == m_selectedKeys.end() )
			{
				m_snappingClosestKey = *m_selectedKeys.rbegin();
			}
			else if( (*rightIt)->getTime() == i.x || rightIt == m_selectedKeys.begin() )
			{
				m_snappingClosestKey = *rightIt;
			}
			else
			{
				auto leftIt = std::prev( rightIt );
				m_snappingClosestKey = fabs( i.x - (*leftIt)->getTime() ) < fabs( i.x - (*rightIt)->getTime() ) ? *leftIt : *rightIt;
			}
			m_snappingClosestKeyTime = m_snappingClosestKey->getTime();
		}

		moveKeyframes( V2f( i.x, i.y ) );
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
			for( auto &key : *curvePlug )
			{
				if( b.intersects( V2f( key.getTime(), key.getValue() ) ) )
				{
					m_selectedKeys.emplace( &key );
				}
			}
		}

		break;
	}
	case DragMode::Moving :
	{
		m_overwrittenKeys.clear();
		break;
	}

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

CurveGadget::CurveGadget( std::string name, const Gaffer::Animation::CurvePlug *curvePlug )
	: m_name( name ), m_curvePlug( curvePlug )
{

	enterSignal().connect( boost::bind( &CurveGadget::enter, this, ::_1, ::_2 ) );
	leaveSignal().connect( boost::bind( &CurveGadget::leave, this, ::_1, ::_2 ) );

}

CurveGadget::~CurveGadget()
{
}

void CurveGadget::doRenderLayer( Layer layer, const Style *style ) const
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

	Animation::ConstKeyPtr previousKey = nullptr;
	V2f previousKeyPosition = V2f( 0 );

	for( const auto &key : *m_curvePlug )
	{
		V2f keyPosition = viewportGadget->worldToRasterSpace( V3f( key.getTime(), key.getValue(), 0 ) );

		if( previousKey )
		{
			// \todo: needs tangent computation/hand-off as soon as we support more interpolation modes
			//        consider passing interpolation into renderCurveSegment to handle all drawing there

			const Imath::Color3f *userColor = colorFromName();

			if( key.getType() == Gaffer::Animation::Linear )
			{
				style->renderCurveSegment( previousKeyPosition, keyPosition, /* inTangent */ V2f( 0 ), /* outTangent */ V2f( 0 ), getHighlighted() ? Style::HighlightedState : Style::NormalState, userColor );
			}
			else if( key.getType() == Gaffer::Animation::Step )
			{
				style->renderLine( IECore::LineSegment3f( V3f( previousKeyPosition.x, previousKeyPosition.y, 0 ), V3f( keyPosition.x, previousKeyPosition.y, 0) ), 0.5, userColor );
				style->renderLine( IECore::LineSegment3f( V3f( keyPosition.x, previousKeyPosition.y, 0 ), V3f( keyPosition.x, keyPosition.y, 0 ) ), 0.5, userColor );
			}
		}

		previousKey = &key;
		previousKeyPosition = keyPosition;
	}

}

std::string CurveGadget::getToolTip( const IECore::LineSegment3f &line ) const
{
	return m_name;
}

void CurveGadget::enter( GadgetPtr gadget, const ButtonEvent &event )
{
	setHighlighted( true );
}

void CurveGadget::leave( GadgetPtr gadget, const ButtonEvent &event )
{
	setHighlighted( false );
}

// \todo: consider making the colorForAxes function in StandardStyle public?
const Imath::Color3f *CurveGadget::colorFromName() const
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
