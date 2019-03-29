#include <string>
#include <cmath>
#include <optional>
#include <functional>

#include <boost/program_options.hpp>
#include <unistd.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/types.h>

namespace bpo = boost::program_options;

class filter {
public:    
    void add_rules(const bpo::variables_map& vm) {
        if (vm.count("size")) {
            std::string str = vm["size"].as<std::string>();
            if (str.size() < 2 || str.size() > std::log10(std::numeric_limits<off_t>::max())
                    || !std::all_of(str.begin() + 1, str.end(), [] (unsigned char c) { return std::isdigit(c); })) {
                throw_size_format_exception();
            }
            
            std::function<bool (off_t, off_t)> cmp;
            off_t size = std::stoll(str.substr(1));
            switch (str[0]) {
                case '-':
                    cmp = std::greater_equal<off_t>();
                    break;
                case '+':
                    cmp = std::less_equal<off_t>();
                    break;
                case '=':
                    cmp = std::not_equal_to<off_t>();
                    break;
                default:
                    throw_size_format_exception();
            }
            
            f.push_back([size, func = cmp] (const struct stat& s) { return !func(s.st_size, size); });
        }
        add(vm, "nlinks", &stat::st_nlink);
        add(vm, "inum", &stat::st_ino);
        if (vm.count("name")) {
            name = std::make_optional(vm["name"].as<std::string>());
        }
        if (vm.count("exec")) {
            exec = std::make_optional(vm["exec"].as<std::string>());
        }
    }

    bool suits(const char* filename, const struct stat& s) const {
        return !(name && *name != filename) && std::all_of(f.begin(), f.end(), [&s] (auto func) { return func(s); });
    }
    
    void execute(char* filepath) const {
        if (exec) {
            char* const argv[] = {const_cast<char*>(exec->data()), filepath, nullptr};
            execute_command(argv, nullptr);
        }
    }
    
    operator bool() const {
        return name || f.size();
    }

private:
    template <typename T>
    void add(const bpo::variables_map& vm, const char* str, T (stat::*ptom)) {
        if (vm.count(str)) {
            f.push_back([n = vm[str].as<int>(), ptom](const struct stat& s) { return s.*ptom == n; });
        }
    }
    
    void throw_size_format_exception() {
        throw bpo::error("size must be in the following format: \"[-=+]size\""); 
    }
    
    void execute_command(char* const argv[], char* const env[]) const {
        switch (pid_t pid = fork()) {
            case -1:
                perror("Can't create a child process");
                break;
            case 0:
                if (execve(argv[0], argv, env) == -1) {
                    perror("Execution failed");
                    exit(-1);
                }
                exit(0);
            default:
                int result;
                if (waitpid(pid, &result, 0) == -1) {
                    perror("Waiting failed");         
                }
        }
    }
    
    std::vector<std::function<bool (const struct stat&)>> f;
    std::optional<std::string> name;
    std::optional<std::string> exec;
};