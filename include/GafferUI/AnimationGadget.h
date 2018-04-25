//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2018, Matti Gruener. All rights reserved.
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

#ifndef GAFFERUI_ANIMATIONGADGET_H
#define GAFFERUI_ANIMATIONGADGET_H

#include "boost/multi_index_container.hpp"
#include <boost/multi_index/ordered_index.hpp>
#include <boost/multi_index/identity.hpp>

#include "Gaffer/Animation.h"

#include "GafferUI/Gadget.h"
#include "GafferUI/ViewportGadget.h"

namespace GafferUI
{

/// Aliases that define the intended use of each
/// Gadget::Layer by the AnimationGadget components.
namespace AnimationLayer
{
	constexpr Gadget::Layer Grid = Gadget::Layer::Back;
	constexpr Gadget::Layer Curves = Gadget::Layer::MidBack;
	constexpr Gadget::Layer Keys = Gadget::Layer::Main;
	constexpr Gadget::Layer Axes = Gadget::Layer::MidFront;
	constexpr Gadget::Layer Overlay = Gadget::Layer::Front;
};

struct KeyPtrLessThan
{
	bool operator()( const Gaffer::Animation::KeyPtr &lhs, const Gaffer::Animation::KeyPtr &rhs ) const
	{
		return lhs->getTime() < rhs->getTime();
	}
};

IE_CORE_FORWARDDECLARE( CurveGadget );

class GAFFERUI_API AnimationGadget : public Gadget
{

public :

	AnimationGadget();

	~AnimationGadget() override;

	IE_CORE_DECLARERUNTIMETYPEDEXTENSION( GafferUI::AnimationGadget, AnimationGadgetTypeId, Gadget );

	void setVisiblePlugs( const std::vector<Gaffer::Plug *> &plugs );
	void setEditablePlugs( const std::vector<Gaffer::Plug *> &plugs );
	void setFrame( float frame );

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

protected :

	void doRenderLayer( Layer layer, const Style *style ) const override;

private :

	void insertKeyframes();
	void removeKeyframes();
	void moveKeyframes( const Imath::V2f currentDragOffset );
	void frame() const;

	void parentChanged( GraphComponent *child, GraphComponent *oldParent );
	void cameraChanged();
	void viewportChanged();

	void plugDirtied( Gaffer::Plug *plug );

	struct AxisDefinition
	{
		std::vector<std::pair<float, float> > main;
		std::vector<float> secondary;
	};

	// \todo make a local helper in anonymous namespace
	void computeGrid( AxisDefinition &x, AxisDefinition &y ) const;

	bool buttonPress( GadgetPtr gadget, const ButtonEvent &event );
	bool buttonRelease( GadgetPtr gadget, const ButtonEvent &event );

	bool keyPress( GadgetPtr gadget, const KeyEvent &event );

	IECore::RunTimeTypedPtr dragBegin( GadgetPtr gadget, const DragDropEvent &event );
	bool dragEnter( GadgetPtr gadget, const DragDropEvent &event );
	bool dragMove( GadgetPtr gadget, const DragDropEvent &event );
	bool dragEnd( GadgetPtr gadget, const DragDropEvent &event );

	ViewportGadget *m_viewportGadget;
	Imath::V2i m_resolution;
	Imath::Box2f m_viewportBounds;

	std::vector<Gaffer::Animation::CurvePlugPtr> m_curvePlugsVisible;
	std::vector<Gaffer::Animation::CurvePlugPtr> m_curvePlugsEditable;
	std::vector<GadgetPtr> m_animationCurves;

	std::set<Gaffer::Animation::KeyPtr, KeyPtrLessThan> m_selectedKeys;

	float m_currentFrame;

	enum class DragMode
	{
		None,
		Selecting,
		Moving
	};

	enum class MoveAxis
	{
		Both,
		Undefined,
		X,
		Y
	};

	Imath::V2f m_dragStartPosition;
	Imath::V2f m_lastDragPosition;
	DragMode m_dragMode;
	MoveAxis m_moveAxis;
	Gaffer::Animation::KeyPtr m_snappingClosestKey;
	double m_xSnappingPreviousOffset;
	std::set<std::pair<Gaffer::Animation::KeyPtr, Gaffer::Animation::CurvePlugPtr> > m_overwrittenKeys;

	float m_snappingClosestKeyTime;
};

IE_CORE_DECLAREPTR( AnimationGadget );

} // namespace GafferUI

#endif // GAFFERUI_ANIMATIONGADGET_H
