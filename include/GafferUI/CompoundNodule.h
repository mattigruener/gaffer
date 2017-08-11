//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2011-2013, Image Engine Design Inc. All rights reserved.
//  Copyright (c) 2012, John Haddon. All rights reserved.
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

#ifndef GAFFERUI_COMPOUNDNODULE_H
#define GAFFERUI_COMPOUNDNODULE_H

#include "Gaffer/Plug.h"
#include "GafferUI/Nodule.h"
#include "GafferUI/LinearContainer.h"

namespace GafferUI
{

IE_CORE_FORWARDDECLARE( NoduleLayout );

/// A Nodule subclass to represent each of the children of a
/// Plug with their own nodule.
class CompoundNodule : public Nodule
{

	public :

		CompoundNodule( Gaffer::PlugPtr plug );
		~CompoundNodule() override;

		IE_CORE_DECLARERUNTIMETYPEDEXTENSION( GafferUI::CompoundNodule, CompoundNoduleTypeId, Nodule );

		bool acceptsChild( const Gaffer::GraphComponent *potentialChild ) const override;

		/// Returns a Nodule for a child of the plug being represented.
		Nodule *nodule( const Gaffer::Plug *plug );
		const Nodule *nodule( const Gaffer::Plug *plug ) const;

		bool canCreateConnection( const Gaffer::Plug *endpoint ) override;
		void createConnection( Gaffer::Plug *endpoint ) override;

	private :

		NoduleLayout *noduleLayout();
		const NoduleLayout *noduleLayout() const;

		static NoduleTypeDescription<CompoundNodule> g_noduleTypeDescription;

};

IE_CORE_DECLAREPTR( CompoundNodule );

typedef Gaffer::FilteredChildIterator<Gaffer::TypePredicate<CompoundNodule> > CompoundNoduleIterator;
typedef Gaffer::FilteredRecursiveChildIterator<Gaffer::TypePredicate<CompoundNodule> > RecursiveCompoundNoduleIterator;

} // namespace GafferUI

#endif // GAFFERUI_COMPOUNDNODULE_H
