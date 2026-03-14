#ifndef AUDIO_LIST_H
#define AUDIO_LIST_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct audio_list_node {
  uint16_t sample;
  struct audio_list_node *next;
};

struct audio_list {
  struct audio_list_node *head;
  struct audio_list_node *tail;
  size_t count;
};

void audio_list_init(struct audio_list *list);
void audio_list_destroy(struct audio_list *list);
bool audio_list_push_tail(struct audio_list *list, uint16_t sample);
bool audio_list_pop_head(struct audio_list *list, uint16_t *out_sample);
size_t audio_list_count(const struct audio_list *list);
const struct audio_list_node *audio_list_head(const struct audio_list *list);
const struct audio_list_node *audio_list_tail(const struct audio_list *list);

#endif