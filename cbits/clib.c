#include "clib.h"
#include <alpm.h>
#include <alpm_list.h>
#include <libgen.h>
#include <string.h>

void dispatch_list(community_list_callback_t callback, const char *name,
                   const char *key, alpm_list_t *src) {
  if (src == NULL)
    return;
  alpm_list_t *l;
  for (l = src; l; l = alpm_list_next(l)) {
    alpm_depend_t *dep = l->data;
    const char *d_ver = dep->version;
    callback(name, key, dep->name, d_ver == NULL ? "" : d_ver);
  }
}

void query_community(community_pkg_callback_t pkg_callback,
                     community_list_callback_t list_callback) {
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
    const char *desc = alpm_pkg_get_desc(i->data);
    const char *url = alpm_pkg_get_url(i->data);
    alpm_list_t *depends = alpm_pkg_get_depends(i->data);
    alpm_list_t *makedepends = alpm_pkg_get_makedepends(i->data);
    alpm_list_t *checkdepends = alpm_pkg_get_checkdepends(i->data);
    alpm_list_t *provides = alpm_pkg_get_provides(i->data);
    alpm_list_t *conflicts = alpm_pkg_get_conflicts(i->data);
    alpm_list_t *replaces = alpm_pkg_get_replaces(i->data);
    alpm_list_t *optdepends = alpm_pkg_get_optdepends(i->data);
    pkg_callback(name, ver, desc, url);
    if (strcmp(name, "ghc") == 0 || strcmp(name, "ghc-libs") == 0) {
      alpm_list_t *v, *provides;
      provides = alpm_pkg_get_provides(i->data);
      for (v = provides; v; v = alpm_list_next(v)) {
        alpm_depend_t *d = v->data;
        const char *d_name = d->name;
        const char *d_ver = d->version;
        // promote
        pkg_callback(d_name, d_ver, "", "");
      }
    }

    dispatch_list(list_callback, name, "depends", depends);
    dispatch_list(list_callback, name, "makedepends", makedepends);
    dispatch_list(list_callback, name, "checkdepends", checkdepends);
    dispatch_list(list_callback, name, "provides", provides);
    dispatch_list(list_callback, name, "conflicts", conflicts);
    dispatch_list(list_callback, name, "replaces", replaces);
    dispatch_list(list_callback, name, "optdepends", optdepends);
  }
  alpm_release(handle);
}

void query_files(const char *dbname, file_callback_t callback) {
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
