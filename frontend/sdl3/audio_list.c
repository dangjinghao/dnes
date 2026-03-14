#include "audio_list.h"
#include <stdlib.h>

void audio_list_init(struct audio_list *list) {
  if (list == NULL) {
    return;
  }
  list->head = NULL;
  list->tail = NULL;
  list->count = 0;
}

void audio_list_destroy(struct audio_list *list) {
  if (list == NULL) {
    return;
  }

  struct audio_list_node *node = list->head;
  while (node != NULL) {
    struct audio_list_node *next = node->next;
    free(node);
    node = next;
  }

  list->head = NULL;
  list->tail = NULL;
  list->count = 0;
}

bool audio_list_push_tail(struct audio_list *list, uint16_t sample) {
  if (list == NULL) {
    return false;
  }

  struct audio_list_node *node =
      (struct audio_list_node *)malloc(sizeof(struct audio_list_node));
  if (node == NULL) {
    return false;
  }

  node->sample = sample;
  node->next = NULL;

  if (list->tail == NULL) {
    list->head = node;
    list->tail = node;
  } else {
    list->tail->next = node;
    list->tail = node;
  }

  list->count++;
  return true;
}

bool audio_list_pop_head(struct audio_list *list, uint16_t *out_sample) {
  if (list == NULL || list->head == NULL) {
    return false;
  }

  struct audio_list_node *node = list->head;
  if (out_sample != NULL) {
    *out_sample = node->sample;
  }

  list->head = node->next;
  if (list->head == NULL) {
    list->tail = NULL;
  }

  free(node);
  list->count--;
  return true;
}

size_t audio_list_count(const struct audio_list *list) {
  if (list == NULL) {
    return 0;
  }
  return list->count;
}

const struct audio_list_node *audio_list_head(const struct audio_list *list) {
  if (list == NULL) {
    return NULL;
  }
  return list->head;
}

const struct audio_list_node *audio_list_tail(const struct audio_list *list) {
  if (list == NULL) {
    return NULL;
  }
  return list->tail;
}