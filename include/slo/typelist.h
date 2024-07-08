#pragma once
#include <array>
#include <algorithm>
#include <cstddef>
#include <tuple>
#include <type_traits>
#include <utility>

// TYPE_AT takes an index and an unexpanded parameter pack
// this is semantically equivalent to __type_pack_element
#if __has_builtin(__type_pack_element)
#  define TYPE_AT(Idx, Pack) __type_pack_element<Idx, Pack...>
#elif __cpp_pack_indexing >= 202311L
#  define TYPE_AT(Idx, Pack) Pack...[Idx]
#else
#  define TYPE_AT(Idx, Pack) \
    decltype(get_type_impl<Idx>(slo::detail::GetHelper<std::make_index_sequence<sizeof...(Pack)>, Pack...>{}))
#endif

namespace slo {
template <typename... Ts>
struct TypeList;

// standard case alias
// you might find that slo::typelist is more pleasant on the eyes
template <typename... Ts>
using type_list = TypeList<Ts...>;

// list operations
namespace list {
template <typename T>
struct SizeOf;

template <typename... Args>
struct SizeOf<TypeList<Args...>> {
  static constexpr std::size_t value = sizeof...(Args);
};

// size
template <typename T>
constexpr inline std::size_t size = SizeOf<T>::value;

// type at index
template <std::size_t Idx, typename... Ts>
using type_at = typename TypeList<Ts...>::template type_at<Idx>;

// index of type
template <typename T, typename... Ts>
constexpr inline std::size_t index_of = TypeList<Ts...>::template index_of<T>;

// concatenation
template <typename... Us>
using concat = decltype((... + Us{}));

// intersperse + intercalate
template <typename, typename>
struct Intercalate;

template <typename X>
struct Intercalate<TypeList<>, X> {
  using type = TypeList<>;
};

template <typename T, typename X>
struct Intercalate<TypeList<T>, X> {
  using type = TypeList<T>;
};

template <typename T, typename... Ts, typename... Xs>
  requires(sizeof...(Ts) >= 1)
struct Intercalate<TypeList<T, Ts...>, TypeList<Xs...>> {
  using type = concat<TypeList<T>, TypeList<Xs..., Ts>...>;
};

template <typename TL, typename... Xs>
using intercalate = typename Intercalate<TL, TypeList<Xs...>>::type;

template <typename TL, typename X>
using intersperse = typename Intercalate<TL, X>::type;

// n-th row
template <std::size_t N, typename>
struct NthRow;

template <std::size_t N>
struct NthRow<N, TypeList<>> {
  using type = TypeList<>;
};

template <std::size_t N, typename... Ts, typename... Rest>
  requires(N >= sizeof...(Ts))
struct NthRow<N, TypeList<TypeList<Ts...>, Rest...>> {
  // index larger than type pack -> skip
  using type = typename NthRow<N, TypeList<Rest...>>::type;
};

template <std::size_t N, typename... Ts, typename... Rest>
  requires(N < sizeof...(Ts))
struct NthRow<N, TypeList<TypeList<Ts...>, Rest...>> {
  // grab item at index, recurse to next type list
  using type = concat<TypeList<type_at<N, Ts...>>, typename NthRow<N, TypeList<Rest...>>::type>;
};

template <std::size_t N, typename T>
using nth_row = typename NthRow<N, T>::type;

// transpose
template <typename T>
static constexpr std::size_t max_list_size = list::size<T>;

template <typename... Ts>
static constexpr std::size_t max_list_size<TypeList<Ts...>> = std::max({list::size<Ts>...});

template <typename T, typename = std::make_index_sequence<max_list_size<T>>>
struct Transpose;

template <typename T, std::size_t... Idx>
struct Transpose<T, std::index_sequence<Idx...>> {
  using type = TypeList<nth_row<Idx, T>...>;
};

template <typename... Ts>
using transpose = typename Transpose<TypeList<Ts...>>::type;

// filter
template <template <typename> class F, typename... Ts>
struct Filter;

template <template <typename> class F>
struct Filter<F> {
  using type = TypeList<>;
};

template <template <typename> class F, typename T, typename... Ts>
struct Filter<F, T, Ts...> {
  using type = std::
      conditional_t<F<T>::value, typename Filter<F, Ts...>::type::template prepend<T>, typename Filter<F, Ts...>::type>;
};

template <template <typename> class F, typename... Ts>
using filter = typename Filter<F, Ts...>::type;

template <typename, typename>
struct MultiplyOne;

template <typename T, typename... Us>
struct MultiplyOne<T, TypeList<Us...>> {
  using type = TypeList<TypeList<T, Us>...>;
};

template <typename... Ts, typename... Us>
struct MultiplyOne<TypeList<Ts...>, TypeList<Us...>> {
  // if the first parameter was a type list append to it
  // this allows chaining without increasing nesting depth
  using type = TypeList<TypeList<Ts..., Us>...>;
};

template <typename... TLs>
using product = decltype((... * TLs{}));
}  // namespace list

namespace detail {
template <std::size_t Idx, typename T>
struct Tagged {
  using type = T;
};

template <std::size_t Idx, typename T>
T get_type_impl(Tagged<Idx, T>) {
  static_assert(false, "get_type_impl not allowed in an evaluated context");
}

template <typename T, std::size_t Idx>
Tagged<Idx, T> get_index_impl(Tagged<Idx, T>) {
  static_assert(false, "get_index_impl not allowed in an evaluated context");
}

template <typename, typename...>
struct GetHelper;

template <std::size_t... Idx, typename... Ts>
struct GetHelper<std::index_sequence<Idx...>, Ts...> : Tagged<Idx, Ts>... {
  static_assert(sizeof...(Ts) == sizeof...(Idx), "Index pack size mismatch");
};

}  // namespace detail

template <typename... Ts>
struct TypeList {
  // size
  static constexpr std::size_t size = sizeof...(Ts);

  // type at index, subscript
  template <std::size_t Idx>
  using type_at = TYPE_AT(Idx, Ts);

  // index of type iff type occurs exactly once
  // otherwise size of the pack (one past highest valid index)
  template <typename T>
  constexpr static std::size_t index_of = sizeof...(Ts);

  template <typename T>
    requires requires(detail::GetHelper<TypeList> obj) { detail::get_index_impl<T>(obj); }
  constexpr static std::size_t index_of<T> = decltype(detail::get_index_impl<T>(detail::GetHelper<TypeList>{}))::value;

  // slicing
  template <std::size_t, typename T>
  struct SliceImpl;

  template <std::size_t Offset, std::size_t... Idx>
  struct SliceImpl<Offset, std::index_sequence<Idx...>> {
    using type = TypeList<type_at<Offset + Idx>...>;
  };

  template <std::size_t Start, std::size_t End = sizeof...(Ts)>
    requires(Start <= sizeof...(Ts) && End <= sizeof...(Ts))
  using slice = typename SliceImpl<Start, std::make_index_sequence<End - Start>>::type;

  // splitting
  template <std::size_t Idx>
  using split = TypeList<slice<0, Idx>, slice<Idx>>;

  // reversing
  template <typename T = std::make_index_sequence<sizeof...(Ts)>>
  struct ReverseImpl;

  template <std::size_t... Idx>
  struct ReverseImpl<std::index_sequence<Idx...>> {
    using type = TypeList<type_at<sizeof...(Idx) - Idx - 1>...>;
  };

  using reverse = typename ReverseImpl<>::type;

  // intersperse and intercalate
  template <typename T>
  using intersperse = typename list::Intercalate<TypeList, T>::type;

  template <typename... Xs>
  using intercalate = typename list::Intercalate<TypeList, TypeList<Xs...>>::type;

  // mutation
  template <typename T>
  using append = TypeList<Ts..., T>;

  template <typename T>
  using prepend = TypeList<T, Ts...>;

  template <std::size_t Count>
  using drop_last = slice<0, sizeof...(Ts) - Count>;

  template <std::size_t Count>
  using drop_first = slice<Count>;

  template <std::size_t Idx, typename T>
  using replace = decltype(slice<0, Idx>{} + TypeList<T>{} + slice<Idx + 1>{});

  template <template <typename...> class To>
  using rename = To<Ts...>;

  // evaluating traits
  template <template <typename> class Trait>
  constexpr static bool all = (Trait<Ts>::value && ...);

  template <template <typename> class Trait>
  constexpr static bool any = (Trait<Ts>::value || ...);

  // map
  template <template <typename> class F>
  using map = TypeList<F<Ts>...>;

  template <template <typename> class F>
  using map_t = TypeList<typename F<Ts>::type...>;

  template <typename F, typename... Args>
  constexpr static auto fmap(F&& callable, Args&&... args) {
    if constexpr (size == 0) {
      // no types to visit, do nothing
      return;
    }

    using return_types = TypeList<decltype(callable.template operator()<Ts>(std::forward<Args>(args)...))...>;
    if constexpr (return_types::template all<std::is_void>) {
      (void)(callable.template operator()<Ts>(std::forward<Args>(args)...), ...);
      return;
    } else if constexpr (return_types::template all<std::is_same, typename return_types::head>) {
      return std::array<typename return_types::head, return_types::size> {
        {callable.template operator()<Ts>(std::forward<Args>(args)...)...}
      };
    } else {
      return std::make_tuple(callable.template operator()<Ts>(std::forward<Args>(args)...)...);
    }
  }

  // invoking callables
  template <typename F, typename... Args>
  constexpr static decltype(auto) invoke(F&& callable, Args&&... args) {
    return std::forward<F>(callable).template operator()<Ts...>(std::forward<Args>(args)...);
  }

  template <typename F, typename... Args>
  constexpr static void for_each(F&& callable, Args&&... args) {
    (callable.template operator()<Ts>(std::forward<Args>(args)...), ...);
  }

  template <typename F, typename... Args>
  constexpr static void enumerate(F&& callable, Args&&... args) {
    [&callable, &args...]<std::size_t... Idx>(std::index_sequence<Idx...>) {
      (callable.template operator()<type_at<Idx>, Idx>(std::forward<Args>(args)...), ...);
    }(std::index_sequence_for<Ts...>{});
  }
};

// concatenation - can be folded over
template <typename... Ts, typename... Us>
auto operator+(TypeList<Ts...>, TypeList<Us...>) -> TypeList<Ts..., Us...>;

// cartesian product - can be folded over
template <typename... Ts, typename... Us>
auto operator*(TypeList<Ts...>,
               TypeList<Us...>) -> list::concat<typename list::MultiplyOne<Ts, TypeList<Us...>>::type...>;

// multiplying any list with an empty list results in an empty list
template <typename... Us>
auto operator*(TypeList<>, TypeList<Us...>) -> TypeList<>;

// types for quoting
template <auto V>
struct Constant {
  static constexpr decltype(V) value = V;
};

template <template <typename...> class T>
struct TypeTemplate {
  template <typename... Ts>
  using type = T<Ts...>;
};

template <template <auto...> class T>
struct ConstantTemplate {
  template <auto... Ts>
  using type = T<Ts...>;
};

// quote helper
template <typename T>
// T is already a type, could alternatively yield std::type_identity<T> here
auto as_type() -> T;

template <auto T>
auto as_type() -> Constant<T>;

template <template <typename...> class T>
auto as_type() -> TypeTemplate<T>;

template <template <auto...> class T>
auto as_type() -> ConstantTemplate<T>;

#define AS_TYPE(x) decltype(as_type<x>())
}  // namespace slo