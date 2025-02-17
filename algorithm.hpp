#pragma once

#if __cplusplus < 202302L
    #error out of date c++ version, compile with -stdc++=2c
#endif

#include <algorithm>
#include <cstdint>
#include <functional>
#include <ranges>
#include <type_traits>
#include <utility>

namespace alg {
    inline namespace orithm {
        namespace detail {
            auto constexpr predicate_always_returning_true = [](auto&& a) { return true; };

            template <typename tp_iterator_t>
            concept iterator_has_base =
                std::input_iterator<tp_iterator_t> &&
                requires (tp_iterator_t p_iterator) { p_iterator.base(); };

            template <iterator_has_base tp_iterator_t>
            using iterator_base_t = decltype(std::declval<tp_iterator_t>().base());

            template <typename tp_type_t>
            concept no_throw_constructable_or_move_only = std::is_nothrow_constructible_v<tp_type_t> || !std::is_copy_constructible_v<tp_type_t>;

            template <bool tp_condition, std::input_iterator tp_iterator_t>
            using move_iterator_if_t = std::conditional_t<
                tp_condition,
                std::move_iterator<std::remove_cvref_t<tp_iterator_t>>,
                std::remove_cvref_t<tp_iterator_t>
            >;

            template <bool tp_condition>
            struct make_move_iterator_if_fn {
                template <std::input_iterator tp_iterator_t>
                auto constexpr operator()(tp_iterator_t p_iterator) const -> move_iterator_if_t<tp_condition, tp_iterator_t> {
                    if constexpr (tp_condition)
                        return std::move_iterator{std::move(p_iterator)};
                    else return p_iterator;
                }                
            };
            template <bool tp_condition>
            auto constexpr make_move_iterator_if = make_move_iterator_if_fn<tp_condition>{};

            struct make_move_iterator_if_value_type_is_noexcept_fn {
                template <std::input_iterator tp_iterator_t>
                auto constexpr operator()(tp_iterator_t p_iterator) const -> move_iterator_if_t<no_throw_constructable_or_move_only<std::iter_value_t<tp_iterator_t>>, tp_iterator_t> {
                    if constexpr (no_throw_constructable_or_move_only<std::iter_value_t<tp_iterator_t>>)
                        return std::move_iterator{std::move(p_iterator)};
                    else return p_iterator;
                }                
            };
            auto constexpr make_move_iterator_if_value_type_is_noexcept = make_move_iterator_if_value_type_is_noexcept_fn{};

            template <bool tp_condition, iterator_has_base tp_iterator_t>
            using iterator_base_if_t = std::conditional_t<
                tp_condition,
                iterator_base_t<tp_iterator_t>,
                std::remove_cvref_t<tp_iterator_t>
            >;

            struct get_iterator_base_if_value_type_is_noexcept_fn {
                template <std::input_iterator tp_iterator_t>
                auto constexpr operator()(tp_iterator_t p_iterator) const -> iterator_base_if_t<no_throw_constructable_or_move_only<std::iter_value_t<tp_iterator_t>>, tp_iterator_t> {
                    if constexpr (no_throw_constructable_or_move_only<std::iter_value_t<tp_iterator_t>>)
                        return p_iterator.base();
                    else return p_iterator;
                }
            };
            auto constexpr get_iterator_base_if_value_type_is_noexcept = get_iterator_base_if_value_type_is_noexcept_fn{};

            struct write_and_increment_iterator_t {
                template <typename tp_input_iterator_t, typename tp_output_iterator_t>
                requires (
                    std::input_iterator<std::remove_cvref_t<tp_input_iterator_t>> &&
                    std::output_iterator<std::remove_cvref_t<tp_output_iterator_t>, std::iter_reference_t<tp_input_iterator_t>>
                )
                auto operator()(tp_output_iterator_t&& p_out, tp_input_iterator_t&& p_in) const -> write_and_increment_iterator_t {
                    *p_out = *p_in;
                    ++p_out;
                    return *this;
                }
            };
            auto constexpr write_and_increment_iterator = detail::write_and_increment_iterator_t{};
        }
        
        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first)
                        detail::write_and_increment_iterator(p_result, p_first);
                    return copy_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (*p_first == p_value)
                            detail::write_and_increment_iterator(p_result, p_first);
                    }
                    return copy_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_while = detail::copy_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first)))
                            detail::write_and_increment_iterator(p_result, p_first);
                    }
                    return copy_if_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_if_while = detail::copy_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_projection, *p_first) == p_value) {
                            detail::write_and_increment_iterator(p_result, p_first);
                            break;
                        }
                    }
                    return copy_first_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first_while = detail::copy_first_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_first_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first = detail::copy_first_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            detail::write_and_increment_iterator(p_result, p_first);
                            break;
                        }
                    }
                    return copy_first_if_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first_if_while = detail::copy_first_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_first_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first_if = detail::copy_first_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    if constexpr (
                        std::same_as<tp_input_iterator1_t, tp_input_iterator2_t> &&
                        std::bidirectional_iterator<tp_input_iterator1_t>
                    ) {
                        auto [l_in, l_out] = copy_first_while(
                            std::reverse_iterator{std::move(p_last)},
                            std::reverse_iterator{std::move(p_first)},
                            std::move(p_result),
                            p_value,
                            std::move(p_predicate),
                            std::move(p_projection)
                        );
                        return copy_last_while_result{std::move(l_in.base()), std::move(l_out)};
                    }
                    else {
                        static_assert(false, "TODO: need to implement forward iterator's branch, but should implement find_while variants first");
                    } 
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last_while = detail::copy_last_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_last_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last = detail::copy_last_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    if constexpr (
                        std::same_as<tp_input_iterator1_t, tp_input_iterator2_t> &&
                        std::bidirectional_iterator<tp_input_iterator1_t>
                    ) {
                        auto [l_in, l_out] = copy_first_if_while(
                            std::reverse_iterator{std::move(p_last)},
                            std::reverse_iterator{std::move(p_first)},
                            std::move(p_result),
                            std::move(p_predicate1),
                            std::move(p_predicate2),
                            std::move(p_projection)
                        );
                        return copy_last_if_while_result{std::move(l_in.base()), std::move(l_out)};
                    }
                    else {
                        static_assert(false, "TODO: need to implement forward iterator's branch, but should implement find_while variants first");
                    } 
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last_if_while = detail::copy_last_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_last_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last_if = detail::copy_last_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::iter_difference_t<tp_input_iterator1_t>{0}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            detail::write_and_increment_iterator(p_result, p_first);
                            n = 0;
                        }
                        else ++n;
                    }
                    return copy_nth_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::iter_difference_t<tp_input_iterator1_t>{0}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            if (std::invoke(p_projection, *p_first) == p_value)
                                detail::write_and_increment_iterator(p_result, p_first);
                            n = 0;
                        }
                        else ++n;
                    }
                    return copy_nth_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_nth_while = detail::copy_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> copy_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> copy_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_nth = detail::copy_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::iter_difference_t<tp_input_iterator1_t>{0}; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first)))
                                detail::write_and_increment_iterator(p_result, p_first);
                            n = 0;
                        }
                        else ++n;
                    }
                    return copy_nth_if_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_nth_if_while = detail::copy_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_nth_if = detail::copy_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::iter_difference_t<tp_input_iterator1_t>{0}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            detail::write_and_increment_iterator(p_result, p_first);
                            break;
                        }
                        else ++n;
                    }
                    return copy_first_nth_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_first_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::iter_difference_t<tp_input_iterator1_t>{0}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            if (std::invoke(p_projection, *p_first) == p_value)
                                detail::write_and_increment_iterator(p_result, p_first);
                            break;
                        }
                        else ++n;
                    }
                    return copy_first_nth_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_first_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first_nth_while = detail::copy_first_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> copy_first_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_first_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> copy_first_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_first_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_first_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_first_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first_nth = detail::copy_first_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_first_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::iter_difference_t<tp_input_iterator1_t>{0}; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first)))
                                detail::write_and_increment_iterator(p_result, p_first);
                            break;
                        }
                        else ++n;
                    }
                    return copy_first_nth_if_while_result{std::move(p_last), std::move(p_result)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_first_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first_nth_if_while = detail::copy_first_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_first_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_first_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_first_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_first_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_first_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_first_nth_if = detail::copy_first_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    if constexpr (
                        std::same_as<tp_input_iterator1_t, tp_input_iterator2_t> &&
                        std::bidirectional_iterator<tp_input_iterator1_t>
                    ) {
                        auto [l_in, l_out] = copy_first_nth_while(
                            std::reverse_iterator{std::move(p_last)},
                            std::reverse_iterator{std::move(p_first)},
                            std::move(p_result),
                            p_n,
                            std::move(p_predicate),
                            std::move(p_projection)
                        );
                        return copy_last_nth_while_result{std::move(l_in.base()), std::move(l_out)};
                    }
                    else {
                        static_assert(false, "TODO: need to implement forward iterator's branch, but should implement find_while variants first");
                    } 
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_last_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    if constexpr (
                        std::same_as<tp_input_iterator1_t, tp_input_iterator2_t> &&
                        std::bidirectional_iterator<tp_input_iterator1_t>
                    ) {
                        auto [l_in, l_out] = copy_first_nth_while(
                            std::reverse_iterator{std::move(p_last)},
                            std::reverse_iterator{std::move(p_first)},
                            std::move(p_result),
                            p_n,
                            p_value,
                            std::move(p_predicate),
                            std::move(p_projection)
                        );
                        return copy_last_nth_while_result{std::move(l_in.base()), std::move(l_out)};
                    }
                    else {
                        static_assert(false, "TODO: need to implement forward iterator's branch, but should implement find_while variants first");
                    } 
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_last_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last_nth_while = detail::copy_last_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> copy_last_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> copy_last_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_last_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_last_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last_nth = detail::copy_last_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_last_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    if constexpr (
                        std::same_as<tp_input_iterator1_t, tp_input_iterator2_t> &&
                        std::bidirectional_iterator<tp_input_iterator1_t>
                    ) {
                        auto [l_in, l_out] = copy_first_nth_if_while(
                            std::reverse_iterator{std::move(p_last)},
                            std::reverse_iterator{std::move(p_first)},
                            std::move(p_result),
                            p_n,
                            std::move(p_predicate1),
                            std::move(p_predicate2),
                            std::move(p_projection)
                        );
                        return copy_last_nth_if_while_result{std::move(l_in.base()), std::move(l_out)};
                    }
                    else {
                        static_assert(false, "TODO: need to implement forward iterator's branch, but should implement find_while variants first");
                    }                 }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_last_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last_nth_if_while = detail::copy_last_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using copy_last_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct copy_last_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> copy_last_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_last_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_copyable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> copy_last_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr copy_last_nth_if = detail::copy_last_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;
        
        namespace detail {
            struct move_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_while = detail::move_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_if_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_if_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_while = detail::move_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if = detail::move_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_first_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first_while = detail::move_first_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_first_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first = detail::move_first_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_if_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_first_if_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first_if_while = detail::move_first_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_first_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first_if = detail::move_first_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_last_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last_while = detail::move_last_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_last_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last = detail::move_last_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_if_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_last_if_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last_if_while = detail::move_last_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_last_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last_if = detail::move_last_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_nth_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_nth_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_nth_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_nth_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_nth_while = detail::move_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> move_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> move_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_nth = detail::move_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_nth_if_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_nth_if_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_nth_if_while = detail::move_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_nth_if = detail::move_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_nth_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_first_nth_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_first_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_nth_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_first_nth_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_first_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first_nth_while = detail::move_first_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> move_first_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_first_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> move_first_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_first_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_first_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_first_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first_nth = detail::move_first_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_first_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_nth_if_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_first_nth_if_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_first_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first_nth_if_while = detail::move_first_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_first_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_first_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_first_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_first_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_first_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_first_nth_if = detail::move_first_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_nth_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_last_nth_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_last_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_nth_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_last_nth_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_last_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last_nth_while = detail::move_last_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> move_last_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> move_last_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_last_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_last_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last_nth = detail::move_last_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_last_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_nth_if_while(
                        std::move_iterator{std::move(p_first)},
                        std::move_iterator{std::move(p_last)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_last_nth_if_while_result{l_in.base(), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_last_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last_nth_if_while = detail::move_last_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_last_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_last_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_last_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_last_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_last_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_last_nth_if = detail::move_last_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;
        
        namespace detail {
            struct move_if_noexcept_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_while = detail::move_if_noexcept_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_if_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_if_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_if_while = detail::move_if_noexcept_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_if = detail::move_if_noexcept_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_first_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first_while = detail::move_if_noexcept_first_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_first_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first = detail::move_if_noexcept_first_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_if_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_first_if_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first_if_while = detail::move_if_noexcept_first_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_first_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_first_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first_if = detail::move_if_noexcept_first_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_last_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last_while = detail::move_if_noexcept_last_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_last_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last = detail::move_if_noexcept_last_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_if_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_last_if_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last_if_while = detail::move_if_noexcept_last_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_last_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_if_noexcept_last_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last_if = detail::move_if_noexcept_last_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_nth_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_nth_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_nth_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_nth_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_nth_while = detail::move_if_noexcept_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> move_if_noexcept_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> move_if_noexcept_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_nth = detail::move_if_noexcept_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_nth_if_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_nth_if_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_nth_if_while = detail::move_if_noexcept_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_nth_if = detail::move_if_noexcept_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_nth_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_first_nth_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_nth_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_first_nth_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first_nth_while = detail::move_if_noexcept_first_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> move_if_noexcept_first_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_first_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> move_if_noexcept_first_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_first_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first_nth = detail::move_if_noexcept_first_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_first_nth_if_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_first_nth_if_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first_nth_if_while = detail::move_if_noexcept_first_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_first_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_first_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_first_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_first_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_first_nth_if = detail::move_if_noexcept_first_nth_if_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_nth_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_nth_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_nth_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_last_nth_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_nth_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_last_nth_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_value_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last_nth_while = detail::move_if_noexcept_last_nth_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_nth_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_nth_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n
                )
                const
                -> move_if_noexcept_last_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        predicate_always_returning_true
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n
                )
                const
                -> move_if_noexcept_last_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n
                    );
                }
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_value_t,
                    typename                                tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    const tp_value_t&                                  p_value,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        p_value,
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range  tp_input_range_t,
                    std::weakly_incrementable tp_output_iterator_t,
                    typename                  tp_value_t,
                    typename                  tp_projection_t  = std::identity
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> &&
                    std::indirect_binary_predicate<
                        std::ranges::equal_to,
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >,
                        std::add_pointer_t<std::add_const_t<tp_value_t>>
                    >
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    const tp_value_t&                                                       p_value,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        p_value,
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last_nth = detail::move_if_noexcept_last_nth_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_nth_if_while_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_nth_if_while_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                        tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate1_t                                    p_predicate1,
                    tp_predicate2_t                                    p_predicate2,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [l_in, l_out] = copy_last_nth_if_while(
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_first)),
                        detail::make_move_iterator_if_value_type_is_noexcept(std::move(p_last)),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_if_noexcept_last_nth_if_while_result{detail::get_iterator_base_if_value_type_is_noexcept(l_in), std::move(l_out)};
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate1_t,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate2_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate1_t                                                         p_predicate1,
                    tp_predicate2_t                                                         p_predicate2,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_if_while_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last_nth_if_while = detail::move_if_noexcept_last_nth_if_while_fn{};

        template <typename tp_in_iterator_t, typename tp_out_iterator_t>
        using move_if_noexcept_last_nth_if_result = std::ranges::in_out_result<tp_in_iterator_t, tp_out_iterator_t>;

        namespace detail {
            struct move_if_noexcept_last_nth_if_fn {
                template <
                    std::input_iterator                     tp_input_iterator1_t,
                    std::sentinel_for<tp_input_iterator1_t> tp_input_iterator2_t,
                    std::weakly_incrementable               tp_output_iterator_t,
                    typename                                tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            tp_input_iterator1_t,
                            tp_projection_t
                        >
                    >                                       tp_predicate_t
                >
                requires (
                    std::indirectly_movable<tp_input_iterator1_t, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_iterator1_t                               p_first,
                    tp_input_iterator2_t                               p_last,
                    tp_output_iterator_t                               p_result,
                    const std::iter_difference_t<tp_input_iterator1_t> p_n,
                    tp_predicate_t                                     p_predicate,
                    tp_projection_t                                    p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_if_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_if_noexcept_last_nth_if_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        predicate_always_returning_true,
                        std::move(p_projection)
                    );
                }
                template <
                    std::ranges::input_range                           tp_input_range_t,
                    std::weakly_incrementable                          tp_output_iterator_t,
                    typename                                           tp_projection_t  = std::identity,
                    std::indirect_unary_predicate<
                        std::projected<
                            std::ranges::iterator_t<tp_input_range_t>,
                            tp_projection_t
                        >
                    >                                                  tp_predicate_t
                >
                requires (
                    std::indirectly_movable<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t>
                )
                auto constexpr operator()(
                    tp_input_range_t&&                                                      p_range,
                    tp_output_iterator_t                                                    p_result,
                    const std::iter_difference_t<std::ranges::iterator_t<tp_input_range_t>> p_n,
                    tp_predicate_t                                                          p_predicate,
                    tp_projection_t                                                         p_projection = {}
                )
                const
                -> move_if_noexcept_last_nth_if_result<std::ranges::iterator_t<tp_input_range_t>, tp_output_iterator_t> {
                    return (*this)(
                        std::ranges::begin(p_range),
                        std::ranges::end(p_range),
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                }
            };
        }
        auto constexpr move_if_noexcept_last_nth_if = detail::move_if_noexcept_last_nth_if_fn{};
    }
}
