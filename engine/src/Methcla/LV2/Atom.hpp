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

#include <cassert>
#include <ostream>
#include <stdexcept>
#include <sstream>
#include <string>

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"

namespace Methcla { namespace LV2 {

    class Property
    {
    public:
        Property(LV2_URID key, LV2_URID context=0)
            : m_key(key)
            , m_context(context)
        { }

        LV2_URID key() const { return m_key; }
        LV2_URID context() const { return m_context; }

    private:
        LV2_URID m_key, m_context;
    };

    class Forge : public LV2_Atom_Forge
    {
    public:
        Forge(const LV2_Atom_Forge& forge)
            : LV2_Atom_Forge(forge)
        { }
        Forge(LV2_URID_Map* uridMap)
        {
            lv2_atom_forge_init(this, uridMap);
        }
        Forge(const Forge& other) = default;

        void setBuffer(uint8_t* data, uint32_t size)
        {
            lv2_atom_forge_set_buffer(this, data, size);
        }

        void atom(uint32_t size, uint32_t type)
        {
            lv2_atom_forge_atom(this, size, type);
        }

        void write(const void* data, uint32_t size)
        {
            lv2_atom_forge_write(this, const_cast<void*>(data), size);
        }

        Forge& operator<<(int32_t x)
        {
            lv2_atom_forge_int(this, x);
            return *this;
        }

        Forge& operator<<(int64_t x)
        {
            lv2_atom_forge_long(this, x);
            return *this;
        }

        Forge& operator<<(float x)
        {
            lv2_atom_forge_float(this, x);
            return *this;
        }

        Forge& operator<<(double x)
        {
            lv2_atom_forge_double(this, x);
            return *this;
        }

        Forge& operator<<(bool x)
        {
            lv2_atom_forge_bool(this, x);
            return *this;
        }

        Forge& operator<<(LV2_URID x)
        {
            lv2_atom_forge_urid(this, x);
            return *this;
        }

        Forge& operator<<(const char* x)
        {
            lv2_atom_forge_string(this, x, strlen(x));
            return *this;
        }

        Forge& operator<<(const std::string& x)
        {
            lv2_atom_forge_string(this, x.c_str(), x.size());
            return *this;
        }

        Forge& operator<<(const class Property& x)
        {
            lv2_atom_forge_property_head(this, x.key(), x.context());
            return *this;
        }
    };

    class Frame : public LV2_Atom_Forge_Frame
    {
    public:
        Frame(Forge& forge)
            : m_forge(forge)
        { }
        virtual ~Frame()
        {
            lv2_atom_forge_pop(&m_forge, this);
        }

        operator Forge& () { return m_forge; }

    protected:
        Forge& m_forge;
    };

    class TupleFrame : public Frame
    {
    public:
        TupleFrame(Forge& forge)
            : Frame(forge)
        {
            lv2_atom_forge_tuple(&m_forge, this);
        }
    };

    class ObjectFrame : public Frame
    {
    public:
        ObjectFrame(Forge& forge, LV2_URID id, LV2_URID otype)
            : Frame(forge)
        {
            lv2_atom_forge_blank(&m_forge, this, id, otype);
        }
    };

    //* Return true if atom is of the specified type.
    bool isa(const LV2_Atom* atom, LV2_URID type)
    {
        return atom->type == type;
    }

    //* Return true if object is of the specified type.
    bool isa(const LV2_Atom_Object* object, LV2_URID otype)
    {
        return object->body.otype == otype;
    }

    class Parser
    {
    public:
        Parser(LV2_URID_Map* uridMap)
        {
            lv2_atom_forge_init(&m_forge, uridMap);
        }
        Parser(const Parser& other) = default;

        bool isBlank(const LV2_Atom* atom) const
        {
            return isa(atom, m_forge.Blank);
        }
        bool isResource(const LV2_Atom* atom) const
        {
            return isa(atom, m_forge.Resource);
        }
        bool isObject(const LV2_Atom* atom) const
        {
            return isBlank(atom) || isResource(atom);
        }
        bool isTuple(const LV2_Atom* atom) const
        {
            return isa(atom, m_forge.Tuple);
        }
        bool isSequence(const LV2_Atom* atom) const
        {
            return isa(atom, m_forge.Sequence);
        }

        bool isBlank(const LV2_Atom_Object* object) const
        {
            return isBlank(reinterpret_cast<const LV2_Atom*>(object));
        }
        bool isResource(const LV2_Atom_Object* object) const
        {
            return isResource(reinterpret_cast<const LV2_Atom*>(object));
        }

        template <typename T> T cast(const LV2_Atom* atom, LV2_URID type, const char* typeName=nullptr) const
        {
            checkType(atom, type, typeName);
            return reinterpret_cast<T>(atom);
        }

        template <typename T> T cast(const LV2_Atom* atom) const
        {
            return static_cast<T>(atom);
        }

        const LV2_Atom_Forge& forge()
        {
            return m_forge;
        }

    private:
        void argumentError(const char* typeName) const
        {
            std::stringstream msg;
            msg << "argument type error";
            if (typeName != nullptr)
                msg << ", expected " << typeName;
            throw std::invalid_argument(msg.str());
        }

        void checkType(const LV2_Atom* atom, LV2_URID type, const char* typeName=nullptr) const
        {
            if (!isa(atom, type)) argumentError(typeName);
        }

    private:
        LV2_Atom_Forge m_forge;
    };

    template <> int32_t Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.Int, LV2_ATOM__Int);
        return reinterpret_cast<const LV2_Atom_Int*>(atom)->body;
    }

    template <> int64_t Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.Long, LV2_ATOM__Long);
        return reinterpret_cast<const LV2_Atom_Long*>(atom)->body;
    }

    template <> float Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.Float, LV2_ATOM__Float);
        return reinterpret_cast<const LV2_Atom_Float*>(atom)->body;
    }

    template <> double Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.Double, LV2_ATOM__Double);
        return reinterpret_cast<const LV2_Atom_Double*>(atom)->body;
    }

    template <> LV2_URID Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.URID, LV2_ATOM__URID);
        return reinterpret_cast<const LV2_Atom_URID*>(atom)->body;
    }

    template <> const char* Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.String, LV2_ATOM__String);
        return reinterpret_cast<const char*>(LV2_ATOM_BODY(atom));
    }

    template <> const void* Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.Chunk, LV2_ATOM__Chunk);
        return LV2_ATOM_BODY(atom);
    }

    template <> const LV2_Atom_Object* Parser::cast(const LV2_Atom* atom) const
    {
        if (!isObject(atom))
            argumentError(LV2_ATOM_PREFIX "Object");
        return reinterpret_cast<const LV2_Atom_Object*>(atom);
    }

    template <> const LV2_Atom_Tuple* Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.Tuple, LV2_ATOM__Tuple);
        return reinterpret_cast<const LV2_Atom_Tuple*>(atom);
    }

    template <> const LV2_Atom_Sequence* Parser::cast(const LV2_Atom* atom) const
    {
        checkType(atom, m_forge.Sequence, LV2_ATOM__Sequence);
        return reinterpret_cast<const LV2_Atom_Sequence*>(atom);
    }

    class Printer : public Parser
    {
    public:
        Printer(LV2_URID_Map* map, const LV2_URID_Unmap* unmap)
            : Parser(map)
            , m_unmap(*unmap)
        { }

        void print(std::ostream& out, const LV2_Atom* atom, size_t level=0)
        {
                   if (isa(atom, forge().Int)) {
                indent(out, level);
                out << cast<int32_t>(atom);
            } else if (isa(atom, forge().Long)) {
                indent(out, level);
                out << cast<int64_t>(atom);
            } else if (isa(atom, forge().Float)) {
                indent(out, level);
                out << cast<float>(atom);
            } else if (isa(atom, forge().Double)) {
                indent(out, level);
                out << cast<double>(atom);
            } else if (isa(atom, forge().String)) {
                indent(out, level);
                out << '"' << cast<const char*>(atom) << '"';
            } else if (isa(atom, forge().URID)) {
                indent(out, level);
                out << '<' << unmap(cast<LV2_URID>(atom)) << '>';
            } else if (isTuple(atom)) {
                indent(out, level);
                out << '[' << std::endl;
                const LV2_Atom_Tuple* tuple = cast<const LV2_Atom_Tuple*>(atom);
                for (LV2_Atom* (iter) = lv2_atom_tuple_begin(tuple);
                     !lv2_atom_tuple_is_end(LV2_ATOM_BODY(tuple), (atom)->size, (iter));
                     (iter) = lv2_atom_tuple_next(iter)) {
                    print(out, iter, level+4);
                    out << std::endl;
                }
                indent(out, level);
                out << ']';
            //} else if (isSequence(atom)) {
            } else if (isObject(atom)) {
                indent(out, level);
                const LV2_Atom_Object* object = cast<const LV2_Atom_Object*>(atom);
                if (isResource(object))
                    out << "<" << unmap(object->body.id) << "> ";
                //else if (isBlank(object))
                    //out << "[] ";
                out << '{' << std::endl;
                indent(out, level+4);
                out << "<rdf:type>:" << std::endl;
                indent(out, level+8);
                out << '<' << unmap(object->body.otype) << '>' << std::endl;
                LV2_ATOM_OBJECT_FOREACH(object, iter) {
                    indent(out, level+4);
                    out << '<' << unmap(iter->key) << ">:" << std::endl;
                    print(out, &iter->value, level+8);
                    out << std::endl;
                }
                indent(out, level);
                out << '}';
            } else {
                indent(out, level);
                out << "<Atom " << atom->type << ">";
            }
        }

    private:
        const char* unmap(LV2_URID urid) const
        {
            return m_unmap.unmap(m_unmap.handle, urid);
        }

        static void indent(std::ostream& stream, size_t n)
        {
            for (size_t i=0; i < n; i++) {
                stream << ' ';
            }
        }

    private:
        LV2_URID_Unmap m_unmap;
    };

    //
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

//class Property
//{
//public:
	//Property(const LV2_Atom_Property_Body* prop)
		//: m_impl(prop)
	//{ }

//private:
	//const LV2_Atom_Property* m_impl;
//};

//class Object
//{
//public:
	//Object(const LV2_Atom_Object* obj)
		//: m_impl(obj)
	//{ }

	//class const_iterator
	//{
	//public:
		//explicit const_iterator(const LV2_Atom_Object_Body* obj)
			//: m_iter(it)
		//{ }

		//operator bool () const { return !lv2_atom_}
	//};

	//const_iterator begin() const { return ObjectIterator(m_impl); }
//private:
	//const LV2_Atom_Object_Body* m_impl;
//};

}; };

#endif // METHCLA_LV2_ATOM_HPP_INCLUDED
