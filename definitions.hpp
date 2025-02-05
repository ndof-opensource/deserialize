#ifndef NDOF_DEFINITIONS_HPP
#define NDOF_DEFINITIONS_HPP
 
#include <ndof/details/object_tree.hpp>
#include <ndof/details/one_of.hpp>

#include <concepts>
#include <typeindex>
#include <functional>
#include <type_traits>
#include <cmath>
#include <cstdint>
#include <map>
#include <set>
#include <unordered_set>
#include <queue>
#include <list>
#include <stack>
#include <forward_list>
#include <optional>
#include <memory>
#include <complex>
#include <cstddef>
#include <variant>
#include <tuple>
#include <utility>
 
namespace ndof {


    // TODO: Include the pmr containers.

    // --------------------------------------------------------------------
    template<typename T, template<typename ...> typename Template>
    struct is_instantiation_of : std::false_type{};


    template<template<typename ...> typename Template, typename ...A>
    struct is_instantiation_of<Template<A...>,Template> : std::true_type {};

    template<typename T, template<typename ...> typename Template>
    constexpr bool is_instantiation_of_v = is_instantiation_of<T,Template>::value;

    template<typename T, template<typename ...> typename Template>
    concept instantiation_of = is_instantiation_of_v<T,Template>;

    // --------------------------------------------------------------------
    enum class Void : char{};

    template<typename T>
    struct convert_void{
        using type = T;
    };

    template<typename T>
    requires (::std::is_void_v<T>)
    struct convert_void<T>{
        using type = Void;
    };

    template<typename T>
    using convert_void_t = typename convert_void<T>::type;

    // --------------------------------------------------------------------
    template<typename T, typename ...Args>
    concept none_of = !((std::is_same_v<T,Args>) || ...);
   
    template<typename T>
    struct is_one_of_tuple {
        static constexpr bool value = false;
    };

    template<typename ...A>
    struct is_one_of_tuple <std::tuple<A...>> {
        template<typename T>
        static constexpr bool value = one_of<T,A...>;
    };

    template<typename T, typename Tuple>
    constexpr bool is_one_of_tuple_v = is_one_of_tuple<Tuple>::template value<T>;

    template<typename T, typename Tuple>
    concept one_of_tuple = is_one_of_tuple_v<T,Tuple>;

    // --------------------------------------------------------------------
    template<typename T>
    using Plain = std::remove_cvref_t<T>;

    // TODO: Rename [P2]
    template<typename T>
    concept member_object_ptr_type = std::is_member_object_pointer_v<T>;

    // TODO: Rename [P2]
    template<typename T>
    concept member_fn_ptr = std::is_member_function_pointer_v<T>;
    
    // --------------------------------------------------------------------
    template<size_t ...I>
    using indices = std::index_sequence<I...>;

    template<typename T>
    std::type_index type_index_v = std::type_index(typeid(T));

 

    // --------------------------------------------------------------------
    template<typename T>
    concept void_type = std::is_void_v<T>;

    template<typename T>
    concept not_void_type = !std::is_void_v<T>;

 
    // --------------------------------------------------------------------
    template<typename T>
    struct is_vector_bytes_t{
        constexpr static bool value = false;
    };

    template<>
    struct is_vector_bytes_t<std::vector<std::byte>> {
        constexpr static bool value = true;
    };

    template<typename T>
    constexpr bool is_vector_bytes_v = is_vector_bytes_t<T>::value;

    template<typename T>
    concept vector_bytes = is_vector_bytes_v<T>;
    
    // --------------------------------------------------------------------
    template<typename T>
    struct is_variant_t{
        constexpr static bool value = false;
    };

    template<typename ...T>
    struct is_variant_t<std::variant<T...>> {
        constexpr static bool value = true;
    };

    template<typename ...A>
    constexpr bool is_variant_v = is_variant_t<A...>::value;

    template<typename ...A>
    concept variant_type = is_variant_v<A...>;
    
    // --------------------------------------------------------------------
    using fundamental_types = std::tuple<
        bool, 
        std::string,
        std::size_t, 
        std::nullptr_t, 

        char, char16_t, char32_t, char8_t,

        signed char,   unsigned char,
        int,           unsigned int,
        short int,     unsigned short int, 
        long int,      unsigned long int,
        long long int, unsigned long long int,

        float, double, long double
    >;

    template<typename T> struct fundamental_t;

    template<typename ... Args>
    struct fundamental_t<std::tuple<Args...>>{
        template<typename T>
        constexpr static bool one_of = one_of<T,Args...>;
    };

    template<typename T>
    concept fundamental = fundamental_t<fundamental_types>::template one_of<T>;

    // --------------------------------------------------------------------

    template<typename T>
    concept complex = instantiation_of<T,std::complex>;

    // --------------------------------------------------------------------
    
    template<typename T>
    struct is_smart_pointer{
        constexpr static bool value = false;
    };

    template<typename T>
    struct is_smart_pointer<std::shared_ptr<T>>{
        constexpr static bool value = true;

        template<typename ...Args>
        constexpr static std::shared_ptr<T>(*make_fn)(Args&&... args) = &std::make_shared;
    };

    // template<typename T>
    // struct is_smart_pointer<std::weak_ptr<T>>{
    //     constexpr static bool value = true;
    // };

    template<typename T>
    struct is_smart_pointer<std::unique_ptr<T>>{
        constexpr static bool value = true;

        template<typename ...Args>
        constexpr static std::unique_ptr<T>(*make_fn)(Args&&... args) = &std::make_unique<T,Args...>;
    };

    // TODO: Add weak pointer?
    
    template<typename T>
    constexpr bool is_smart_pointer_v = is_smart_pointer<T>::value;

    template<typename T>
    concept smart_pointer = is_smart_pointer<T>::value;


    template<typename T>
    concept pointer_type = 
           std::is_pointer_v        <T> 
        || is_smart_pointer_v       <T> 
        || std::is_member_pointer_v <T>;
        
    // TODO: Extend to r-value references.

    // --------------------------------------------------------------------
    // https://github.com/schaumb/is_lambda-cpp-type-trait/blob/master/is_lambda.hpp
 

    // --------------------------------------------------------------------
    template<typename T>
    concept prohibited = 
        // pointer_type<T>                || 
        std::is_union_v<T>                ||
        // is_function_like_v<T>          ||
        std::is_same_v<T, std::nullptr_t>;

    // --------------------------------------------------------------------
    template<typename T>
    concept optional_type = is_instantiation_of_v<T,std::optional>;

    // --------------------------------------------------------------------
    template<typename T>
    concept forward_list_type = is_instantiation_of_v<T,std::forward_list>;
    
    // --------------------------------------------------------------------
    template<typename T>
    concept object_tree_type = std::same_as<T,ObjectTree>;

    // --------------------------------------------------------------------
    template<typename T> concept uses_emplace_back  = requires (T t, typename T::value_type v){ t.emplace_back(v);  };
    template<typename T> concept uses_emplace       = requires (T t, typename T::value_type v){ t.emplace(v);       };
    
    
    template<typename T> concept injectible =
        (!object_tree_type<T>) && 
        (!is_vector_bytes_v<T>) && (
            uses_emplace_back  <T> 
            || (
                uses_emplace   <T> && 
                !optional_type <T> 
            )
        );

    template<typename T>
    struct InjectionMethod{};

    template<uses_emplace_back T>
    struct InjectionMethod<T>{
        constexpr static typename T::value_type& (T::*inject)(typename T::value_type&&) = &T::emplace_back;
    };

    template<uses_emplace T>
    struct InjectionMethod<T>{
        using Iterator = typename T::iterator;
        using P = std::pair<Iterator,bool>;
        constexpr static InjectionMethod<T>::P (T::*inject)(typename T::value_type &&) = &T::emplace;
    };

    // --------------------------------------------------------------------
    template<typename T>
    struct bounded_array_t{
        constexpr static bool value = false;
    };

    template<typename T, std::size_t N>
    struct bounded_array_t<T[N]>{
        using type = T;
        constexpr static const std::size_t size = N;
        constexpr static bool value = true;
    };
 
    template<typename T, std::size_t N>
    struct bounded_array_t<std::array<T,N>>{
        using type = T;
        constexpr static const std::size_t size = N;
        constexpr static bool value = true;
    };

    template<typename T>
    constexpr bool bounded_array_v = bounded_array_t<T>::value;

    template<typename T>
    concept bounded_array = bounded_array_v<T>;

    // --------------------------------------------------------------------
    template<typename T>
    struct is_tuple{
        static const constexpr bool value = false;
    };
    
    template<typename ...Args>
    struct is_tuple<std::tuple<Args...>>{
        static const constexpr bool value = true;
    };

    template<typename T, size_t N>
    struct is_tuple<std::array<T,N>>{
        static const constexpr bool value = true;
    };

    template<typename T>
    constexpr bool is_tuple_v = is_tuple<T>::value;

    // BUG: This is inconsistent with line 321.  Which is it?
    template<typename T>
    concept tuple_type = is_tuple_v<T> && !bounded_array_v<T>;

    // --------------------------------------------------------------------
    template<typename T>
    concept enumeration = std::is_enum_v<T>;

    template<typename T>
    concept has_undefined = std::is_enum_v<T> &&
        requires { 
            requires
                T::undefined == (T)0 || 
                T::Undefined == (T)0 || 
                T::UNDEFINED == (T)0; 
        };

    template<has_undefined T>
    T get_undefined() { return (T)0; }
    

    template<typename T>
    constexpr bool has_undefined_v = has_undefined<T>;


    // --------------------------------------------------------------------
    template<typename  T>
    concept pair_type = instantiation_of<T,std::pair>;

    // --------------------------------------------------------------------
    template<typename T>
    struct is_std_array{
        constexpr static bool value = false;
    };

    template<typename T, size_t N>
    struct is_std_array<std::array<T,N>>{
        constexpr static bool value = true;
    };

    template<typename T>
    constexpr bool is_std_array_v = is_std_array<T>::value;

    template<typename T>
    concept array_type = is_std_array_v<T>;

    // --------------------------------------------------------------------
    template<typename T>
    concept aggregate = 
        std::is_aggregate_v<T>             && 
        std::is_default_constructible_v<T> && 
        !std::is_union_v<T>                &&
        !optional_type<T>                  &&
        !bounded_array_v<T>                &&
        !is_object_tree_v<T>               &&
        !array_type<T>;

    // --------------------------------------------------------------------
    template<typename T>
    concept numeric =
         ( std::is_integral_v<T>  || std::is_floating_point_v<T> ) &&  
        !( std::is_same_v<T,bool> || std::is_enum_v<T>           );

    // TODO: Constrain this to only floating point?
    template<typename T> 
    requires std::is_floating_point_v<T>
    bool is_zero(T t){ return std::fpclassify(t) == FP_ZERO; }

    template<typename T> 
    requires std::is_integral_v<T>
    bool is_zero(T t){ return t == T(0); }

    template<typename T>
    struct LargestNumericType;

    template<std::signed_integral T>
    struct LargestNumericType<T>{
        using type = std::intmax_t;
    };

    template<std::unsigned_integral T>
    struct LargestNumericType<T>{
        using type = std::uintmax_t;
    };

    // TODO: float128_t is probably the biggest floating point type.
    template<std::floating_point T>
    struct LargestNumericType<T>{
        using type = long double;
    };

    template<numeric T>
    using largest_numeric_type_t  = typename LargestNumericType<T>::type;

    // --------------------------------------------------------------------
    template<typename T>
    struct TypeTag{
        using type = T;
    };

    template<typename F>
    concept void_no_params_fn = requires (F f){
        requires std::is_same_v<void,decltype(f())>;
    };

    template<typename F>
    concept bool_no_params_fn = requires (F f){
        {f()} -> std::same_as<bool>;
    };
 
    // --------------------------------------------------------------------
    template<typename T>
    struct is_numeric_or_string_t{
        static bool constexpr value = false;
    };

    template<typename T>
    requires std::integral<T> || std::floating_point<T> || std::same_as<std::string,T>
    struct is_numeric_or_string_t<T>{
        static bool constexpr value = true;
    };

    template<typename T>
    static bool constexpr is_numeric_or_string_v = is_numeric_or_string_t<T>::value;

    template<typename T>
    concept numeric_or_string_type = is_numeric_or_string_v<T>;

}

#endif
