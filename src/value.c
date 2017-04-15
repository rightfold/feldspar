#include <stdint.h>
#include <stdlib.h>

typedef struct fs_lay {
  size_t ptr_count;
  size_t aux_count;
  struct fs_lay *ptrs[];
} fs_lay;

fs_lay *fs_alloc(size_t ptr_count, size_t aux_count) {
  size_t i;
  fs_lay *lay;

  lay = malloc(sizeof(fs_lay) + sizeof(fs_lay *) * ptr_count + aux_count);
  lay->ptr_count = ptr_count;
  lay->aux_count = aux_count;
  for (i = 0; i < ptr_count; ++i) {
    lay->ptrs[i] = NULL;
  }
  return lay;
}

void fs_dealloc(fs_lay *lay) {
  free(lay);
}

size_t fs_ptr_count(fs_lay *lay) {
  return lay->ptr_count;
}

size_t fs_aux_count(fs_lay *lay) {
  return lay->aux_count;
}

fs_lay *fs_get_ptr(fs_lay *lay, size_t offset) {
  return lay->ptrs[offset];
}

void fs_set_ptr(fs_lay *lay, size_t offset, fs_lay *ptr) {
  lay->ptrs[offset] = ptr;
}

uint8_t *fs_aux(fs_lay *lay) {
  return (uint8_t *)(lay->ptrs + lay->ptr_count);
}
