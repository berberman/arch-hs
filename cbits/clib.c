#include "clib.h"
#include <alpm.h>
#include <alpm_list.h>
#include <libgen.h>
#include <string.h>

void query_community(callback_t callback) {
  alpm_errno_t err;
  alpm_handle_t *handle;
  handle = alpm_initialize("/", "/var/lib/pacman", &err);
  alpm_db_t *db =
      alpm_register_syncdb(handle, "community", ALPM_SIG_USE_DEFAULT);
  alpm_list_t *i, *pkgs = NULL;

  pkgs = alpm_db_get_pkgcache(db);

  for (i = pkgs; i; i = alpm_list_next(i)) {
    const char *name = alpm_pkg_get_name(i->data);
    const char *ver = alpm_pkg_get_version(i->data);
    if (strcmp(name, "ghc") == 0 || strcmp(name, "ghc-libs") == 0) {
      alpm_list_t *v, *provides;
      provides = alpm_pkg_get_provides(i->data);
      for (v = provides; v; v = alpm_list_next(v)) {
        alpm_depend_t *d = v->data;
        const char *d_name = d->name;
        const char *d_ver = d->version;
        callback(d_name, d_ver);
      }
    } else
      callback(name, ver);
  }
  alpm_release(handle);
}

void query_files(const char *dbname, callback_t callback) {
    alpm_errno_t err;
    alpm_handle_t *handle;
    handle = alpm_initialize("/", "/var/lib/pacman", &err);
    alpm_option_set_dbext(handle, ".files");
    alpm_db_t *db = alpm_register_syncdb(handle, dbname, ALPM_SIG_USE_DEFAULT);
    alpm_list_t *pkg, *pkgs = NULL;

    pkgs = alpm_db_get_pkgcache(db);

    for (pkg = pkgs; pkg; pkg = alpm_list_next(pkg)) {
        const char *pkg_name = alpm_pkg_get_name(pkg->data);
        alpm_filelist_t *pkg_files = alpm_pkg_get_files(pkg->data);
        for (size_t file_num = 0; file_num < pkg_files->count; file_num++) {
            alpm_file_t *file = pkg_files->files + file_num;
            char *path = file->name;
            if (strncmp("usr/lib/", path, 8) != 0)
                continue;
            size_t n = strlen(path);
            if (path[n - 3] != '.')
                continue;
            if (!((path[n - 2] == 's' && path[n - 1] == 'o') ||
                  (path[n - 2] == 'p' && path[n - 1] == 'c')))
                continue;

            callback(pkg_name, basename(path));
        }
    }
    alpm_release(handle);
}
