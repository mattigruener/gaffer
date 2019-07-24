//////////////////////////////////////////////////////////////////////////
//
//  Copyright (c) 2019, Image Engine Design Inc. All rights reserved.
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

#ifndef IECORESCENEPREVIEW_CAPTURINGRENDERER_H
#define IECORESCENEPREVIEW_CAPTURINGRENDERER_H

#include "GafferScene/Private/IECoreScenePreview/Renderer.h"

#include "tbb/concurrent_hash_map.h"

#include <atomic>
#include <unordered_map>

namespace IECoreScenePreview
{

/// A "Renderer" which just captures the scene passed to it, and
/// keeps a history of any interactive edits made. Useful for testing
/// renderer output code.
class IECORESCENE_API CapturingRenderer : public Renderer
{

	public :

		IE_CORE_DECLAREMEMBERPTR( CapturingRenderer )

		CapturingRenderer( RenderType type = RenderType::Interactive, const std::string &fileName = "" );

		/// Introspection
		/// =============

		class CapturedAttributes : public AttributesInterface
		{

			public :

				IE_CORE_DECLAREMEMBERPTR( CapturedAttributes );

				const IECore::CompoundObject *attributes() const;

			private :

				CapturedAttributes( const IECore::ConstCompoundObjectPtr &attributes );

				friend class CapturingRenderer;

				IECore::ConstCompoundObjectPtr m_attributes;

		};

		IE_CORE_DECLAREPTR( CapturedAttributes );

		class CapturedObject : public ObjectInterface
		{

			public :

				IE_CORE_DECLAREMEMBERPTR( CapturedObject )

				~CapturedObject() override;

				/// Introspection
				/// =============

				const std::vector<IECore::ConstObjectPtr> &capturedSamples() const;
				const std::vector<float> &capturedSampleTimes() const;

				const CapturedAttributes *capturedAttributes() const;
				const ObjectSet *capturedLinks( const IECore::InternedString &type ) const;

				int numAttributeEdits() const;
				int numLinkEdits( const IECore::InternedString &type ) const;

				/// Renderer interface
				/// ==================

				void transform( const Imath::M44f &transform ) override;
				void transform( const std::vector<Imath::M44f> &samples, const std::vector<float> &times ) override;
				bool attributes( const AttributesInterface *attributes ) override;
				void link( const IECore::InternedString &type, const ConstObjectSetPtr &objects ) override;

			private :

				CapturedObject( CapturingRenderer *renderer, const std::string &name, const std::vector<const IECore::Object *> &samples, const std::vector<float> &times );

				friend class CapturingRenderer;

				CapturingRenderer *m_renderer;
				const std::string m_name;
				const std::vector<IECore::ConstObjectPtr> m_capturedSamples;
				const std::vector<float> m_capturedSampleTimes;
				ConstCapturedAttributesPtr m_capturedAttributes;
				int m_numAttributeEdits;
				std::unordered_map<IECore::InternedString, std::pair<ConstObjectSetPtr, int>> m_capturedLinks;

		};

		IE_CORE_DECLAREPTR( CapturedObject );

		const CapturedObject *capturedObject( const std::string &name ) const;

		/// Renderer interface
		/// ==================

		IECore::InternedString name() const override;
		void option( const IECore::InternedString &name, const IECore::Object *value ) override;
		void output( const IECore::InternedString &name, const IECoreScene::Output *output ) override;
		AttributesInterfacePtr attributes( const IECore::CompoundObject *attributes ) override;
		ObjectInterfacePtr camera( const std::string &name, const IECoreScene::Camera *camera, const AttributesInterface *attributes ) override;
		ObjectInterfacePtr light( const std::string &name, const IECore::Object *object, const AttributesInterface *attributes ) override;
		ObjectInterfacePtr lightFilter( const std::string &name, const IECore::Object *object, const AttributesInterface *attributes ) override;
		ObjectInterfacePtr object( const std::string &name, const IECore::Object *object, const AttributesInterface *attributes ) override;
		ObjectInterfacePtr object( const std::string &name, const std::vector<const IECore::Object *> &samples, const std::vector<float> &times, const AttributesInterface *attributes ) override;
		void render() override;
		void pause() override;

	private :

		void checkPaused() const;

		std::atomic_bool m_rendering;
		using ObjectMap = tbb::concurrent_hash_map<std::string, const CapturedObject *>;
		ObjectMap m_capturedObjects;

		static Renderer::TypeDescription<CapturingRenderer> g_typeDescription;

};

IE_CORE_DECLAREPTR( CapturingRenderer )

} // namespace IECoreScenePreview

#endif // IECORESCENEPREVIEW_CAPTURINGRENDERER_H
