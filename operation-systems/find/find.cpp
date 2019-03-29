#include <iostream>
#include <string>
#include <cstring>
#include <queue>

#include <unistd.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <boost/program_options.hpp>
#include "filter.cpp"

namespace bpo = boost::program_options;

void process(const filter& f, struct stat& statbuf, char* pathname, const char* filename) {
    if (!f.suits(filename, statbuf)) {
        return;
    }
    printf("%s\n", pathname);
    f.execute(pathname);
}

void find(std::string path, const filter& f) {
    std::queue<std::string> dirs;
    dirs.push(path);
    
    while (!dirs.empty()) {
        const char* dir_name = dirs.front().c_str();
        DIR* dir = opendir(dir_name);
        if (dir == NULL) {
            perror(dir_name);
        } else {
            struct dirent* dp;
            
            while (dp = readdir(dir)) {
                char* file_name = dp->d_name;
                if (!file_name || strcmp(file_name, ".") == 0 || strcmp(file_name, "..") == 0) {
                    continue;
                }
                
                struct stat statbuf;
                std::string fullpath = dirs.front() + "/" + file_name;
                if (lstat(fullpath.c_str(), &statbuf) == -1) {
                    perror(fullpath.c_str());
                    continue;
                }
                
                if (S_ISDIR(statbuf.st_mode)) {
                    dirs.push(fullpath);
                } else if (S_ISREG(statbuf.st_mode)) {
                    process(f, statbuf, fullpath.data(), file_name);
                }
            }
            closedir(dir);
        }
        dirs.pop();
    }
}

int main(int argc, const char *argv[]) {
    bpo::variables_map vm;
    bpo::options_description desc{"Usage"};
    desc.add_options()
        ("help,h", "Help screen")
        ("inum", bpo::value<int>(), "Inode number")
        ("name", bpo::value<std::string>(), "File name")
        ("size", bpo::value<std::string>(), "[-=+] Size filter (less, equals, greater) in bytes")
        ("nlinks", bpo::value<int>(), "File hard link quantity")
        ("exec", bpo::value<std::string>(), "Executable file path (getting files as only argument one by one)")
        ("path", bpo::value<std::string>()->default_value("."), "Path to search in");
    bpo::positional_options_description pos_desc;
    pos_desc.add("path", -1);
    bpo::command_line_parser parser{argc, argv};
    parser.options(desc).positional(pos_desc);
    
    filter f;
    
    try {
        bpo::parsed_options parsed_options = parser.run();
        store(parsed_options, vm);

        if (vm.count("help")) {
            std::cout << desc;
            return 0;
        }
        
        f.add_rules(vm);
        if (!f) {
            std::cerr << "Nothing to search for" << '\n';
            return -1;
        }
    } catch (const bpo::error& ex) {
        std::cerr << ex.what() << '\n';
        return -1;
    }
    
    find(vm["path"].as<std::string>(), f);
    return 0;
}
