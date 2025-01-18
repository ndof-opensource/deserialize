#ifndef NDOF_DESERIALIZE_HPP
#define NDOF_DESERIALIZE_HPP

#include "ndof/details/format.hpp"
#include "ndof/details/string_helpers.hpp"
#include "ndof/details/object_tree.hpp"
// #include "ndof/details/definitions.hpp"
#include "ndof/details/pretty_type_name.hpp"
#include "ndof/details/enum_parser.hpp"

#include "ndof/utils.hpp"
#include <boost/pfr.hpp>
 
#include <vector>
#include <memory>
#include <type_traits>
#include <tuple>
#include <limits>
#include <cstdint>
#include <ranges>
#include <utility>
#include <functional>
#include <vector> 
#include <string_view>
#include <optional>
#include <complex>
#include <concepts>

// TODO: Look into std::pmr containers! Oof.
// TODO: Support binary types with base64 into byte array.
// TODO: Hide the details in implementation files where possible.
// TODO: Add conditions checks.
// TODO: Add predefined constants for opencv and eigen
//       https://github.com/opencv/opencv/blob/e3ae36dcb3c1d523802f8642e5c3984db43637c4/modules/python/src2/defs
//       https://eigen.tuxfamily.org/dox/Constants_8h_source.html
// TODO: Add tools to validate configuration. 
//       Load shared objects and verify all types and predefined variables exist.
// TODO: bitset (map from string ).  
// TODO: NaN and Infinity are optional. Need to check if platform supports these values before using them.
// TODO: Port as many of these specializations as possible to the cpp file.
// TODO: Add support for std::chrono::duration.
// TODO: Add support for std::chrono::time_point.
// TODO: Add support for std::filesystem::path.
// TODO: Add support for wide strings  create a "String" concept. 
// TODO: 
// TODO: 


using namespace std::string_view_literals;

namespace ndof{


#pragma region Forward declarations.

    using ByteVector = ::std::vector<::std::byte>;
    template<typename T>
    concept bool_type = std::same_as<T,bool>;

    template<typename T>
    concept string_type = std::same_as<T,std::string>;
    
    // Forwrard declare these and put them in the cpp or private headers.
    template<aggregate         T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<bool_type         T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<bounded_array     T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<complex           T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<enumeration       T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<forward_list_type T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<injectible        T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<numeric           T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<object_tree_type  T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<optional_type     T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<prohibited        T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<smart_pointer     T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<string_type       T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<tuple_type        T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<variant_type      T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);
    template<vector_bytes      T> [[nodiscard]] bool deserialize (const ObjectTree& tree, T&);

    template<aggregate T, size_t I> 
    [[nodiscard]] bool deserialize_object_field( const ObjectTree& tree, T& );
    
    template<tuple_type Tuple, size_t ...ints> 
    [[nodiscard]] bool deserialize_tuple_sequence( const ObjectTree& tree, Tuple&, std::index_sequence<ints...> i );

    template<tuple_type Tuple, size_t I>
    [[nodiscard]] bool deserialize_at( const ObjectTree& tree, Tuple& );

    bool is_tree_empty( const ObjectTree& tree );

#pragma endregion Forward declarations. 

    template<prohibited T>
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T&){
        static_assert( !std::is_void_v                    <T>, "Void cannot be deserialized."                     );
        static_assert( !std::is_union_v                   <T>, "Unions cannot be deserialized."                   );
        static_assert( !std::is_pointer_v                 <T>, "Pointers cannot be deserialized."                 );
        static_assert( !std::is_null_pointer_v            <T>, "Null pointer cannot be deserialized."             );
        static_assert( !std::is_member_object_pointer_v   <T>, "Member object pointers cannot be deserialized."   );
        static_assert( !std::is_member_function_pointer_v <T>, "Member function pointers cannot be deserialized." );

        return false;
    }

    // Test and deserialize a string.
    template<typename T>
    [[nodiscard]] bool deserialize_simple(const ObjectTree& tree, T& t, bool(ObjectTree::*check_tree_fn )() const noexcept){
        return  evaluate((tree.*check_tree_fn )()).then( [&]{ tree.get_to(t); } );
    }

    
    template<complex T> 
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        using type = typename T::value_type;
        struct { type real; type imag; } complex_numeric;
        return deserialize(tree,complex_numeric).then([&]{ t = std::complex{ complex_numeric.real, complex_numeric.imag }; });
    }

    template<pair_type T> 
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        // TODO: Simplify by reusing aggregate.
        using _1st = std::remove_cv_t<typename T::first_type >;
        using _2nd = std::remove_cv_t<typename T::second_type>;
        // TODO: Consider string_view of const char. 
        //       ...in a separate struct outside of the function.
        static constexpr std::string first  = boost::pfr::get_name<0,T>();
        static constexpr std::string second = boost::pfr::get_name<1,T>();
        return (
            (tree.size() == 2) &&
            (
                (
                    tree.is_array() &&
                    deserialize<_1st>( tree.at( 0 ),   t.first  ) &&
                    deserialize<_2nd>( tree.at( 1 ),   t.second ) 
                ) ||
                (
                    tree.is_object () &&
                    tree.contains  ( first ) &&
                    deserialize<_1st>( tree[ first  ], t.first ) &&
                    tree.contains  ( second ) &&
                    deserialize<_2nd>( tree[ second ], t.second )
                )
            )
        );
    }

    // Test and deserialize an optional field.
    template<optional_type T>
    [[nodiscard]] bool deserialize(const ObjectTree& tree, T& t){
        using ElementType = typename T::value_type;
        ElementType v;
        return (
            ( evaluate(tree.empty() || tree.is_null()).then([&]{ t = std::nullopt; }) ) ||
            ( evaluate(deserialize<ElementType>( tree, v )).then([&]{ t.emplace(v); } ) )
        );
    }

    // deserialize an injectible type.
    // This includes vector, deque, stack, queue, priority_queue, list,
    //    set, unordered_set, multiset, unordered_multiset,
    //    map, unordered_map, multimap, unordered_multimap, 
    template<injectible T>
    [[nodiscard]] bool deserialize(const ObjectTree& tree, T& t ){
        using ValueType  = typename T::value_type;
        return (
            tree.size() == 0 ||
            (
                tree.is_array() &&
                std::all_of( tree.begin(), tree.end(), [&]( auto& child ) {
                    // TODO: Consolidate this by creating a type adjustor for pair<T,U>
                    auto inject = [&](auto& w){
                        return evaluate(deserialize ( child, w )).then( [&] { 
                                ( t.*(InjectionMethod<T>::inject) )( std::move(w) ); 
                            } );
                    };
                    if constexpr(pair_type<ValueType>){
                        using _1st = std::remove_cv_t<typename ValueType::first_type>;
                        using _2nd = typename ValueType::second_type;
                        std::pair<_1st, _2nd> p;
                        return inject(p);
                    }
                    else{
                        ValueType v;
                        return inject(v);
                    }
                })
            )
        ); 
    }

    // deserialize a forward_list<T>. The list needs to be reversed after being constructed.
    template<forward_list_type T>
    [[nodiscard]] bool deserialize(const ObjectTree& tree, T& t){
        using ValueType  = typename T::value_type;
        return (
            std::ranges::all_of(tree,[&t](auto& item){
                ValueType v;
                return ( deserialize<ValueType>( item, v ).then([&]{ t.emplace_front(std::move(item)); }) );
            }).then([&t] { t.reverse(); })
        );
    }

    // deserialize a numeric type, including chars stored as a string or 8-bit type.
    template<numeric T>
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        // TODO: Add support for wchar, wstring, and char16_t, char32_t.
        if constexpr(one_of<T,char,unsigned char>){
            return (
                tree.is_string() &&
                then([&]{
                    auto s = tree.template get<std::string>();
                    return and_op( s.size() == 1, [&]{t = s[0];} );
                })
            );
        }
        else{
            return 
                evaluate(
                    ( tree.is_number_float  () && std::is_floating_point_v <T> ) ||
                    ( tree.is_number_integer() && std::is_integral_v       <T> )
                ).then([&]{
                    using LNT = largest_numeric_type_t<T>;
                    std::optional<LNT> value = tree.template get<LNT>();
                    return 
                        evaluate(
                            value.has_value()
                            && *value >= std::numeric_limits<T>::min()
                            && *value <= std::numeric_limits<T>::max()
                        ).then([&]{ t = *value; });})
                || evaluate(tree.is_string() && std::numeric_limits<T>::has_infinity)
                    && (evaluate("Infinity"sv.compare( tree.template get<std::string_view>() ) == 0)
                            .then( [&]{ t = std::numeric_limits<T>::infinity(); } )
                        || evaluate("-Infinity"sv.compare( tree.template get<std::string_view>() ) == 0)
                            .then( [&]{ t = -std::numeric_limits<T>::infinity(); } ));
        }
    }
    
    // deserialize a single tuple element by position from array.
    template<tuple_type Tuple, size_t I>
    [[nodiscard]] bool deserialize_at(const ObjectTree& tree, Tuple& t){
        using ElementType = std::remove_cvref_t<std::tuple_element_t<I, Tuple>>;
        auto iter = tree.begin() + I;
        return deserialize<ElementType>(*iter,std::get<I>(t));
    }

    // deserialize all fields of a tuple by position stored as json array.
    template<tuple_type Tuple, size_t ...ints>
    [[nodiscard]] bool deserialize_tuple_sequence(const ObjectTree& tree, Tuple& t, std::index_sequence<ints...> i){
        return (deserialize_at<Tuple,ints>(tree, t) && ...);
    }

    // deserialize tuple.
    template<tuple_type T>
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        constexpr static const size_t size = std::tuple_size_v<T>;
        constexpr static const auto   seq  = std::make_index_sequence<size>();
        return (
            tree.is_array() &&
            size == tree.size() &&
            deserialize_tuple_sequence<T>(tree,t,seq)
        ); 
    }
 
    // deserialize object field of object stored as json object, accessed by name.
    template<aggregate T, size_t I>
    [[nodiscard]] bool deserialize_object_field(const ObjectTree& tree, T& t){
        using FieldType = boost::pfr::tuple_element_t<I,T>;
        auto iter = tree.find(boost::pfr::get_name<I,T>());
        return (
            ( iter != tree.end() && deserialize( *iter,boost::pfr::get<I,T>( t ) ) ) ||
            ( iter == tree.end() && is_optional_v<FieldType> )
        );
    }

    // deserialize object fields stored as a json object, accessed by name.
    template<aggregate T, size_t ...I>
    [[nodiscard]] bool deserialize_object_fields(const ObjectTree& tree, T& t, std::index_sequence<I...> seq){
        bool result =  (deserialize_object_field<T,I>(tree,t) && ...);
        return result;
    }

    // deserialize a single value of a field of an object.
    template<aggregate T, size_t I>
    [[nodiscard]] bool deserialize_field_from_array(const ObjectTree& tree, T& t){
        return deserialize<boost::pfr::tuple_element_t<I,T>>(*(tree.begin()+I),boost::pfr::get<I,T>(t));
    }

 

    // Objects stored in array.
    template<aggregate T, size_t ...I >
    [[nodiscard]] bool deserialize_object_as_array (  
        const ObjectTree&     tree, 
        T&                    t, 
        const indices<I...>&  i
    ) {
        return (boost::pfr::tuple_size_v<T> == 0) || (deserialize_field_from_array<T,I>(tree,t) && ...);
    }

    // For objects.
    template<aggregate T>
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        constexpr const static size_t size = boost::pfr::tuple_size_v<T>;
        constexpr const static auto   seq  = std::make_index_sequence<size>();
        // TODO: Count the number of optionals?=
        return evaluate(tree.is_object() && deserialize_object_fields<T>(tree, t, seq))
                .otherwise( tree.size() == size && tree.is_array() && deserialize_object_as_array<T>(tree, t, seq));
    }

    template<object_tree_type T>
    [[nodiscard]] bool deserialize(const ObjectTree& tree, T& t){
        t = tree;
        return true;
    }

    

    template<enumeration T>
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        using ValueType = std::underlying_type_t<T>;
        std::vector<std::string> values;
        return 
            (evaluate( tree.empty() || tree.is_null())
            .then(has_undefined_v<T>)
            .then([&]{if constexpr(has_undefined_v<T>){ t = get_undefined<T>();}}))
            
            || (evaluate(tree.is_string())
                .then( 
                    evaluate(tree.template get<std::string_view>().find("|"sv) != std::string_view::npos ))
                    .then([&]{
                        const auto enums = split_string( tree.template get<std::string_view>(), "|" );
                        if (enums.size() <2) {return false;}
                        ObjectTree temp_tree = ObjectTree::array();
                        std::ranges::for_each(enums,[&](auto& str){temp_tree.emplace_back(str);});
                        return deserialize(temp_tree,t);  
                    })
                    .otherwise(
                        evaluate(tree.template get<std::string_view>().find("|"sv) == std::string_view::npos )
                        .then ([&] {
                            ValueType v = (ValueType)0;
                            std::optional<T> cast_v;
                            return evaluate(convert_to_integral(tree.template get<std::string_view>(), v))
                                    .then([&]{ t = (T)v; })
                                    .otherwise( 
                                        evaluate([&] {
                                            cast_v = enum_parsing::enum_cast<T>(tree.template get<std::string_view>()); 
                                            return cast_v.has_value();
                                        })
                                        .then([&] {t = cast_v.value();})
                                    );
                        })
                    )
                )
                .otherwise(
                    evaluate(tree.is_array())
                    .then([&]{
                            ValueType acc;
                            return evaluate(
                                std::ranges::all_of(
                                    tree,
                                    [&](auto& item){ 
                                        T value;
                                        return evaluate(deserialize(item,value)).then([&]{ acc|= (ValueType)value; });
                                    }))
                            .then([&]{ t = (T)acc; });
                    })
                    .otherwise(
                        evaluate(tree.is_number())
                        .then([&]{
                            ValueType v;
                            return evaluate(deserialize<ValueType>( tree, v ))
                                .then([&]{
                                    auto result = enum_parsing::enum_cast<T>( v );
                                    return evaluate(result.has_value()).then( [&] { t = *result; } );
                                });
                        })
                    )
                );
    }

    template<smart_pointer T>    
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t_ptr){
        return 
            (tree.empty() || tree.is_null()) ||
            evaluate([&]{
                using element_type = typename T::element_type;
                element_type t;
                return evaluate(deserialize(tree, t)).then([&]{
                        // TODO: Verify move is necessary here.
                        t_ptr = is_smart_pointer<T>::template  make_fn<decltype(t)>(std::move(t));
                    });
            });
    }

    template<typename T, typename U, std::size_t I, std::size_t N>
    requires (I==N) 
    [[nodiscard]] bool deserialize_array_elements(const ObjectTree& tree, T& t){
        return true;
    }

    template<typename T, typename U, std::size_t I, std::size_t N>
    requires (I!=N)
    [[nodiscard]] bool deserialize_array_elements(const ObjectTree& tree, T& t){
        return deserialize<U>(*(tree.begin()+I),t[I]) && deserialize_array_elements<T,U,I+1,N>(tree,t);
    }

    template<bounded_array T>    
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        using type = typename bounded_array_t<T>::type;
        constexpr std::size_t size = bounded_array_t<T>::size; 
        return tree.is_array()     
            && tree.size() == size 
            && deserialize_array_elements<T,type,0,size>(tree, t);
    }


    template<variant_type T, size_t N, size_t I>
    requires (I == N)
    [[nodiscard]] bool deserialize_variant(const ObjectTree& tree, T& t){
        return false;
    }
    
    template<variant_type T, size_t N, size_t I>
    requires (I < N)
    [[nodiscard]] bool deserialize_variant(const ObjectTree& tree, T& t){
        using TestType = std::variant_alternative_t<I, T>;
        TestType test_obj;
        return 
            evaluate(deserialize<TestType>(tree, test_obj)).then([&]{ t = std::move(test_obj); })
            || deserialize_variant<T, N, I + 1>(tree, t);
    }

    template<variant_type T>
    [[nodiscard]] bool deserialize (const ObjectTree& tree, T& t){
        return !tree.empty() && !tree.is_null() && deserialize_variant<T,std::variant_size_v<T>,0>(tree,t);
    }



}

#endif
