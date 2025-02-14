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
        }
        
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
                    for (; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        *p_result = std::move(*p_first);
                        ++p_result;
                    }
                    return move_while_result{std::move(p_last), std::move(p_result)};
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
                    for (; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (*p_first == p_value) {
                            *p_result = std::move(*p_first);
                            ++p_result;
                        }
                    }
                    return move_while_result{std::move(p_last), std::move(p_result)};
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
                    for (; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            *p_result = std::move(*p_first);
                            ++p_result;
                        }
                    }
                    return move_if_while_result{std::move(p_last), std::move(p_result)};
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
                    for (; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_projection, *p_first) == p_value) {
                            *p_result = std::move(*p_first);
                            ++p_result;
                            break;
                        }
                    }
                    return move_first_while_result{std::move(p_last), std::move(p_result)};
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
                    for (; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            *p_result = std::move(*p_first);
                            ++p_result;
                            break;
                        }
                    }
                    return move_first_if_while_result{std::move(p_last), std::move(p_result)};
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
                    auto [in, out] = move_first_while(
                        std::reverse_iterator{std::move(p_last)},
                        std::reverse_iterator{std::move(p_first)},
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return move_last_while_result{std::move(in.base()), std::move(out)};
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
                    auto [in, out] = move_first_if_while(
                        std::reverse_iterator{std::move(p_last)},
                        std::reverse_iterator{std::move(p_first)},
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_last_if_while_result{std::move(in.base()), std::move(out)};
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            *p_result = std::move(*p_first);
                            ++p_result;
                            n = 0;
                        }
                        ++n;
                    }
                    return move_nth_while_result{std::move(p_last), std::move(p_result)};
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
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_projection, *p_first) == p_value) {
                            if (n == p_n) {
                                *p_result = std::move(*p_first);
                                ++p_result;
                                n = 0;
                            }
                            ++n;
                        }
                    }
                    return move_nth_while_result{std::move(p_last), std::move(p_result)};
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
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            if (n == p_n) {
                                *p_result = std::move(*p_first);
                                ++p_result;
                                n = 0;
                            }
                            ++n;
                        }
                    }
                    return move_nth_if_while_result{std::move(p_last), std::move(p_result)};
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
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            *p_result = std::move(*p_first);
                            ++p_result;
                            break;
                        }
                        ++n;
                    }
                    return move_first_nth_while_result{std::move(p_last), std::move(p_result)};
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
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_projection, *p_first) == p_value) {
                            if (n == p_n) {
                                *p_result = std::move(*p_first);
                                ++p_result;
                                break;
                            }
                            ++n;
                        }
                    }
                    return move_first_nth_while_result{std::move(p_last), std::move(p_result)};
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
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_first_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            if (n == p_n) {
                                *p_result = std::move(*p_first);
                                ++p_result;
                                break;
                            }
                            ++n;
                        }
                    }
                    return move_first_nth_if_while_result{std::move(p_last), std::move(p_result)};
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
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return move_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
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
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
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
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
                )
                const
                -> move_last_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [in, out] = move_first_nth_if(
                        std::reverse_iterator{std::move(p_last)},
                        std::reverse_iterator{std::move(p_first)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return move_last_nth_if_while_result{std::move(in.base()), std::move(out)};
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
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    for (; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        *p_result = *p_first;
                        ++p_result;
                    }
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
                        if (*p_first == p_value) {
                            *p_result = *p_first;
                            ++p_result;
                        }
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
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            *p_result = *p_first;
                            ++p_result;
                        }
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
                            *p_result = *p_first;
                            ++p_result;
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
                            *p_result = *p_first;
                            ++p_result;
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
                    auto [in, out] = copy_first_while(
                        std::reverse_iterator{std::move(p_last)},
                        std::reverse_iterator{std::move(p_first)},
                        std::move(p_result),
                        p_value,
                        std::move(p_predicate),
                        std::move(p_projection)
                    );
                    return copy_last_while_result{std::move(in.base()), std::move(out)};
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
                    auto [in, out] = copy_first_if_while(
                        std::reverse_iterator{std::move(p_last)},
                        std::reverse_iterator{std::move(p_first)},
                        std::move(p_result),
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return copy_last_if_while_result{std::move(in.base()), std::move(out)};
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            *p_result = *p_first;
                            ++p_result;
                            n = 0;
                        }
                        ++n;
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_projection, *p_first) == p_value) {
                            if (n == p_n) {
                                *p_result = *p_first;
                                ++p_result;
                                n = 0;
                            }
                            ++n;
                        }
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            if (n == p_n) {
                                *p_result = *p_first;
                                ++p_result;
                                n = 0;
                            }
                            ++n;
                        }
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (n == p_n) {
                            *p_result = *p_first;
                            ++p_result;
                            break;
                        }
                        ++n;
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_projection, *p_first) == p_value) {
                            if (n == p_n) {
                                *p_result = *p_first;
                                ++p_result;
                                break;
                            }
                            ++n;
                        }
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_first_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    for (auto n = std::uintmax_t{1}; p_first != p_last && std::invoke(p_predicate2, std::invoke(p_projection, *p_first)); ++p_first) {
                        if (std::invoke(p_predicate1, std::invoke(p_projection, *p_first))) {
                            if (n == p_n) {
                                *p_result = *p_first;
                                ++p_result;
                                break;
                            }
                            ++n;
                        }
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    return copy_last_nth_while(
                        std::move(p_first),
                        std::move(p_last),
                        std::move(p_result),
                        p_n,
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
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_nth_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
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
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    const tp_value_t&    p_value,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
                )
                const
                -> copy_last_nth_if_while_result<tp_input_iterator1_t, tp_output_iterator_t> {
                    auto [in, out] = copy_first_nth_if(
                        std::reverse_iterator{std::move(p_last)},
                        std::reverse_iterator{std::move(p_first)},
                        std::move(p_result),
                        p_n,
                        std::move(p_predicate1),
                        std::move(p_predicate2),
                        std::move(p_projection)
                    );
                    return copy_last_nth_if_while_result{std::move(in.base()), std::move(out)};
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
                    const std::uintmax_t p_n,
                    tp_predicate1_t      p_predicate1,
                    tp_predicate2_t      p_predicate2,
                    tp_projection_t      p_projection = {}
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
                    tp_input_iterator1_t p_first,
                    tp_input_iterator2_t p_last,
                    tp_output_iterator_t p_result,
                    tp_predicate_t       p_predicate,
                    const std::uintmax_t p_n,
                    tp_projection_t      p_projection = {}
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
                    tp_input_range_t&&   p_range,
                    tp_output_iterator_t p_result,
                    const std::uintmax_t p_n,
                    tp_predicate_t       p_predicate,
                    tp_projection_t      p_projection = {}
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
    }
}
