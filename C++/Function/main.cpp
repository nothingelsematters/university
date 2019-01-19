#include "function.h"

#include <iostream>
#include <functional>
#include <vector>


void func(int a) {
    std::cout << "test function " << a << "\n";
}

void func_empty() {
    std::cout << "empty func_test\n";
}

class FuncStruct {
public:
    void operator()() const {
        std::cout << "test class member\n";
    }
};

class FuncStructAnother {
public:
    void operator()() const {
        std::cout << "test class operator()\n";
    }
};

int summarizing(int a, int b) {
    return a + b;
}

template<typename T>
void with_templates(const T& arg) {
    std::cout << "HEY\n";
    std::cout << arg;
}




void test_empty() {
    function<void ()> f;
    std::cout << "empty test: " << static_cast<bool>(f) << '\n';
}

void test_null() {
    function<void ()> f(nullptr);
    std::cout << "nullptr test: " << static_cast<bool>(f) << '\n';
}

void test_function() {
    function<void (int)> f1(func);
    f1(1);
}

void test_structure() {
    FuncStructAnother fuun;
    function<void ()> f2(fuun);
    f2();
}

void test_lambda() {
    function<void ()> f3([]() {
        std::cout << "test lambda function\n";
    });
    f3();
}

void test_member() {
    function<void (FuncStruct)> f4(&FuncStruct::operator());
    FuncStruct fuck;
    f4(fuck);
}

void test_sum() {
    function<int (int, int)> f5(summarizing);
    int a = 5;
    int b = 10;
    std::cout << "test sum: " << a << " + " << b << ' ' << f5(a, b) << '\n';
}

void test_bool() {
    function<void ()> f;
    function<void (FuncStruct)> f4(&FuncStruct::operator());
    FuncStruct funct;
    f4(funct);

    std::cout << "test bool: empty " << static_cast<bool>(f) << '\n'
              << "test bool: function " << static_cast<bool>(f4) << '\n';
}

void test_templates() {
    function<void (std::string)> f(with_templates<std::string>);
    f(std::string("test with templates"));
}

template<typename R, typename... Args>
void move_test_handler(function<R (Args...)>&& func, Args... args) {
    std::cout << "test move: " << func(args...) << '\n';
}

void test_move() {
    function<int (int, int)> f1(summarizing);
    move_test_handler(std::move(f1), 5, 8);
}

template<typename R, typename... Args>
void copy_test_handler(function<R (Args...)> func, Args... args) {
    std::cout << "test copy: " << func(args...) << '\n';
}

void test_copy() {
    function<int (int, int)> f1(summarizing);
    copy_test_handler(f1, 5, 8);
}

void test_swap() {
    FuncStructAnother fuun;
    function<void ()> f2(fuun);

    function<void ()> f3(func_empty);
    std::cout << "swap test: \n was: ";
    f3();
    f2();
    f3.swap(f2);
    std::cout << "now: ";
    f3();
    f2();
}

void test_bind() {
    function<void ()> binder = std::bind(func, 5);
    std::cout << "bind test: ";
    binder();
}

void test_bind_with_params() {
    using namespace std::placeholders;
    function<void (int)> binder = std::bind(func, _1);
    std::cout << "bind test with parametres: ";
    binder(5);
}



int main() {
    test_empty();
    test_null();
    test_function();
    test_structure();
    test_member();
    test_lambda();
    test_sum();
    test_bool();
    test_move();
    test_copy();
    test_swap();
    test_bind();
    test_bind_with_params();
}
