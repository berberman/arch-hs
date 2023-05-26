#ifndef CLIB_H
#define CLIB_H

typedef void file_callback_t(const char *pkg_name, const char *file_path);
typedef void extra_pkg_callback_t(const char *pkg_name, const char *version,
                                      const char *desc, const char *url);
typedef void extra_list_callback_t(const char *pkg_name, const char *key,
                                       const char *dependent_pkg_name,
                                       const char *dependent_pkg_version);

void query_extra(extra_pkg_callback_t pkg_callback, extra_list_callback_t list_callback);

void query_files(const char *dbname, file_callback_t callback);

#endif
