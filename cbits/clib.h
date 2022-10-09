#ifndef CLIB_H
#define CLIB_H

typedef void file_callback_t(const char *pkg_name, const char *file_path);
typedef void community_pkg_callback_t(const char *pkg_name, const char *version,
                                      const char *desc, const char *url);
typedef void community_list_callback_t(const char *pkg_name, const char *key,
                                       const char *dependent_pkg_name,
                                       const char *dependent_pkg_version);

void query_community(community_pkg_callback_t pkg_callback, community_list_callback_t list_callback);

void query_files(const char *dbname, file_callback_t callback);

#endif
