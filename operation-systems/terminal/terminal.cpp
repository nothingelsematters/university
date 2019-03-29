#include <iostream>
#include <string>
#include <cstring>
#include <sstream>
#include <iterator>
#include <vector>
#include <map>
#include <algorithm>

#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>


extern char** environ;
static std::map<std::string, std::string> environment;


void add_environment(char* newenv) {
    std::string env(newenv);
    std::string::size_type index = env.find('=');
    
    if (index == std::string::npos) {
        std::cout << "Can't add " << env << ": no \'=\' found" << '\n';
        return;
    }
    environment[env.substr(0, index)] = env.substr(index + 1);
}

std::vector<char*> cstring_vector(const std::vector<std::string>& v) {
    std::vector<char*> result;
    for (const std::string& str: v) {
        char* res = new char[str.size() + 1];
        strcpy(res, str.data());
        result.push_back(res);
    }
        
    result.push_back(nullptr);
    return result;
}

std::vector<std::string> string_environment() {
    std::vector<std::string> env;
    std::transform(environment.begin(), environment.end(), std::back_inserter(env),
        [] (const std::pair<std::string, std::string>& p) { return p.first + "=" + p.second; });
    return env;
}

std::vector<char*> char_environment() {
    return cstring_vector(string_environment());
}

std::vector<char*> parse_arguments(std::string const& command) {
    std::istringstream iss(command);
    std::vector<std::string> strings{std::istream_iterator<std::string>(iss), 
        std::istream_iterator<std::string>()};
    return cstring_vector(strings);
}

void execute_command(char* argv[], char* env[]) {
    switch (pid_t pid = fork()) {
        case -1:
            std::cout << "Can't create a child process" << '\n';
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
            } else {
                std::cout << "Return code: " << result << '\n';
            }
    }
}

void change_directory(const char* directory) {
    int result = chdir(directory);
    if (result != 0) {
        perror(directory);
    }
}

bool compare_command(const std::vector<char*>& v, const std::string& command) {
    return v.size() > 1 && command == v[0];
}

void handle_command(std::string const& command) {
    std::vector<char*> arguments = parse_arguments(command);
    if (arguments.size() == 1) {
        return;
    }
    
    if (compare_command(arguments, "cd")) {
        change_directory(arguments[1]);
        
    } else if (compare_command(arguments, "export")) {
        if (arguments.size() == 2) {
            std::vector<std::string> env = string_environment();
            std::copy(env.begin(), env.end(), std::ostream_iterator<std::string>(std::cout, "\n"));
            
        } else if (arguments.size() == 3) {
            for (size_t i = 1; i < arguments.size() - 1; ++i) {
                add_environment(arguments[i]);
            }
        }
    
    } else if (compare_command(arguments, "unset")) {
        if (arguments.size() != 3) {
            std::cout << "Usage: unset [environment variable name]" << '\n';
        } else {
            environment.erase(arguments[1]);
        }
        
    } else {
        char** argv = arguments.data();
        std::vector<char*> envir = char_environment();
        char** envp = envir.data();
        execute_command(argv, envp);
        for (auto& e: envir) {
            delete[] e;
        }
    }
    
    for (auto& a: arguments) {
        delete[] a;
    }
}


int main(int argc, char const *argv[]) {
    // copying launching terminal environments
    for (char** current = environ; *current; ++current) {
        add_environment(*current);
    }
    
    if (argc == 2) {
        change_directory(argv[1]);
    }
    
    while (true) {
        char cwd[128];
        if (getcwd(cwd, 100) == NULL) {
            perror("Getting current directory failed");
            std::cout << "$ ";
        } else {
            printf("%s $ ", cwd);
        }
        std::cout.flush();
        
        std::string command;
        std::getline(std::cin, command);
        if (std::cin.eof() || command == "exit") {
            std::cout << "exiting...\n";
            break;
        }
        
        handle_command(command);
    }
    return 0;
}