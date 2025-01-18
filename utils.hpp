#ifndef NDOF_UTILS_HPP
#define NDOF_UTILS_HPP

#include "ndof/details/callable_traits.hpp"
#include "ndof/details/definitions.hpp"

#include <ndof/error.hpp>

#include <tuple>
#include <concepts>
#include <type_traits>
#include <concepts>
#include <utility>
#include <string_view>
#include <vector>

#include <sstream>

#include <boost/pfr/tuple_size.hpp>`
#include <boost/pfr.hpp>

namespace ndof
{

    template <typename T>
    struct TempSharedPtr
    {
        TempSharedPtr(std::weak_ptr<T> ptr)
        {
            preconditions([&]
                          { check_condition(bool(_ptr = ptr.lock()), "Weak pointer is expired."); });
        }

        [[nodiscard]] operator T &() { return _ptr.operator T &(); }
        [[nodiscard]] T &operator*() { return _ptr.operator*(); }
        [[nodiscard]] T *operator->() { return _ptr.operator->(); }

    private:
        std::shared_ptr<T> _ptr;
    };

    std::string_view trim_whitespace(std::string_view);

    template <std::integral T>
    bool convert_to_integral(std::string_view strView, T &t)
    {
        std::istringstream iss(std::string(trim_whitespace(strView.data())));
        iss >> std::noskipws >> t;
        return !(iss.fail() || iss.bad());
    }

    // Move to cpp.
    struct ExpressionResult
    {
        bool expression_value;

        ExpressionResult operator!() const { return ExpressionResult(!expression_value); }

        operator bool() const { return expression_value; }

        constexpr ExpressionResult(bool value) : expression_value(value) {}

        template <bool n>
        constexpr bool conditionally_negate(bool b) const
        {
            if constexpr (n)
                return !b;
            else
                return b;
        }

        // RVO & NRVO LESSON HERE.
        template <bool n = false>
        ExpressionResult then(const Callable auto &f)
        {
            ExpressionResult result(expression_value);
            bool value = conditionally_negate<n>(expression_value);
            if constexpr (std::is_void_v<decltype(f())>)
            {
                if (value)
                    f();
            }
            else
            {
                if (value)
                    result = ExpressionResult(f());
            }
            return result;
        }

        template <bool n = false>
        ExpressionResult then(const auto &v)
            requires std::is_convertible_v<decltype(v), bool>
        {
            return then<n>(v);
        }

        ExpressionResult otherwise(const Callable auto &&f)
        {
            return then<true>(std::forward < decltype<f>(f));
        }

        ExpressionResult otherwise(const auto &&v)
            requires std::is_convertible_v<decltype(v), bool>
        {
            return otherwise(std::forward < decltype<v>(v));
        }

        // Add And, Or.
    };
    // finally() returns a Finally().  evaluate returns a PostEval(). Otherwise, Finally and Then are sub-PostEvals. ExpressionResult has finally, then, and otherwise.

    auto evaluate(const auto &expression)
        requires std::is_convertible_v<decltype(expression), bool>
    {
        return ExpressionResult(expression);
    }

    auto evaluate(const auto &f)
        requires(!std::is_void_v<decltype(f())> && std::is_convertible_v<decltype(f()), bool>)
    {
        return ExpressionResult(f());
    }

    template <Callable F>
        requires(!std::is_void_v<typename CallableTraits<F>::ReturnType>)
    std::optional<typename CallableTraits<F>::ReturnType> noexcept_fn(F &&f) noexcept
    {
        std::optional<typename CallableTraits<F>::ReturnType> result;
        try
        {
            result.emplace(std::forward<F>(f)());
        }
        catch (...)
        {
            // Do nothing.
        }
        return result;
    }

    template <StandaloneFunction F>
        requires(std::is_void_v<typename CallableTraits<F>::ReturnType>)
    std::optional<Void> noexcept_fn(F &f) noexcept
    {
        std::optional<Void> result;
        try
        {
            result.emplace((f(), Void()));
        }
        catch (...)
        { /*Do nothing.*/
        }
        return result;
    }

#endif
