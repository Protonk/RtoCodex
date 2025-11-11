// Optional C++ file to exercise C++20 and OpenMP when toggled
#include <vector>
#include <numeric>
#include <type_traits>

#ifdef ENABLE_CPP20
  #include <span> // C++20
  template <typename T>
  concept Integral = std::is_integral_v<T>;
  template <Integral T>
  T sum_span(std::span<const T> s) {
  #ifdef ENABLE_OPENMP
    T total = 0;
    #pragma omp parallel for reduction(+:total)
    for (ptrdiff_t i = 0; i < static_cast<ptrdiff_t>(s.size()); ++i) {
      total += s[static_cast<size_t>(i)];
    }
    return total;
  #else
    T total = 0;
    for (auto v : s) total += v;
    return total;
  #endif
  }
#else
  static int cpp_stub_sum(const std::vector<int>& v) {
  #ifdef ENABLE_OPENMP
    int total = 0;
    #pragma omp parallel for reduction(+:total)
    for (size_t i = 0; i < v.size(); ++i) total += v[i];
    return total;
  #else
    int total = 0;
    for (size_t i = 0; i < v.size(); ++i) total += v[i];
    return total;
  #endif
  }
#endif
// No exported symbols referenced from R. File exists to enforce toolchain features.
