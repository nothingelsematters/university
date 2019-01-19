#ifndef FUNCTION_H
#define FUNCTION_H

#include <cstddef>
#include <memory>
#include <variant>

#include <iostream>

template <class>
class function;

template<typename R, typename... Args>
class function<R (Args...)> {
public:
    function() noexcept : callable() {}

    function(std::nullptr_t) noexcept : callable(nullptr) {}

    function(const function& other) : callable(other.callable->clone()) {}

    function(function&& other) noexcept {
        swap(other);
        other.callable = nullptr;
    }

    template <typename F>
    function(F f) {
        std::cout << "size " << sizeof(f) << "        ";
        if (sizeof(f) > MAX_SIZE) {
            std::cout << " using BIG OBJECT ";
            callable = std::make_unique<pointer_holder<F>>(f);
        } else {
            std::cout << " using SMALL OBJECT ";
            callable = std::make_unique<function_holder<F>>(f);
        }
    }

    template <typename F, typename Class>
    function(F Class::* member) : callable(std::make_unique<class_holder<F, Class>>(member)) {}

    ~function() {}

    function& operator=(const function& other) {
        function<R (Args...)> new_func(other);
        swap(new_func);
        return *this;
    }

    function& operator=(function&& other) noexcept {
        swap(other);
        return *this;
    }

    void swap(function& other) noexcept {
        std::swap(callable, other.callable);
    }

    explicit operator bool() const noexcept {
        return static_cast<bool>(callable);
    }

    R operator()(Args... args) const {
        return callable->call(args...);
    }


private:

    class callable_holder_base {
    public:
        callable_holder_base() {}

        virtual ~callable_holder_base() {}

        virtual R call(Args... args) const = 0;

        virtual std::unique_ptr<callable_holder_base> clone() const = 0;

        callable_holder_base(const callable_holder_base&) = delete;
        callable_holder_base& operator=(const callable_holder_base&) = delete;
    };

    typedef std::unique_ptr<callable_holder_base> callable_t;

    template <typename F>
    class function_holder : public callable_holder_base {
    public:
        function_holder(F func) : callable_holder_base(), func(func) {}

        R call(Args... args) const {
            return func(args...);
        }

        callable_t clone() const {
            return std::unique_ptr<function_holder>(new function_holder(func));
        }

    private:
        F func;
    };

    template <typename F>
    class pointer_holder : public callable_holder_base {
    public:
        pointer_holder(F func) : callable_holder_base(), func(new F(func)) {}

        R call(Args... args) const {
            return (*func)(args...);
        }

        callable_t clone() const {
            return std::unique_ptr<pointer_holder>(new pointer_holder(*func));
        }

    private:
        std::unique_ptr<F> func;
    };

    template <typename F, typename Class, typename... FunArgs>
    class class_holder : public callable_holder_base {
    public:
        typedef F Class::* member;

        class_holder(member func) : callable_holder_base(), func(func) {}

        R call(Class object, FunArgs... fun_args) const {
            return (object.*func)(fun_args...);
        }

        callable_t clone() const {
            return std::unique_ptr<class_holder>(new class_holder(func));
        }

    private:
        member func;
    };

    callable_t callable;
    static const int MAX_SIZE = 10;
};

#endif // FUNCTION_H
