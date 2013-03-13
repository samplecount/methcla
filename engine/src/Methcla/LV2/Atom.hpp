// Copyright 2012-2013 Samplecount S.L.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef METHCLA_LV2_ATOM_HPP_INCLUDED
#define METHCLA_LV2_ATOM_HPP_INCLUDED

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

namespace LV2 {

// class ObjectIterator
//   : public boost::iterator_facade<
//         ObjectIterator
//       , Property const
//       , boost::input_iterator_tag
//     >
// {
//  public:
//     ObjectIterator()
//       : m_node(0) {}

//     explicit ObjectIterator(node_base* p)
//       : m_node(p) {}

//  private:
//     friend class boost::iterator_core_access;

//     void increment() { m_node = m_node->next(); }

//     bool equal(const_node_iterator const& other) const
//     {
//         return this->m_node == other.m_node;
//     }

//     node_base const& dereference() const { return *m_node; }

//     node_base const* m_node;
// };

class Property
{
public:
	Property(const LV2_Atom_Property_Body* prop)
		: m_impl(prop)
	{ }

private:
	const LV2_Atom_Property* m_impl;
};

class Object
{
public:
	Object(const LV2_Atom_Object* obj)
		: m_impl(obj)
	{ }

	class const_iterator
	{
	public:
		explicit const_iterator(const LV2_Atom_Object_Body* obj)
			: m_iter(it)
		{ }

		operator bool () const { return !lv2_atom_}
	};

	const_iterator begin() const { return ObjectIterator(m_impl); }
private:
	const LV2_Atom_Object_Body* m_impl;
};

};

#endif // METHCLA_LV2_ATOM_HPP_INCLUDED