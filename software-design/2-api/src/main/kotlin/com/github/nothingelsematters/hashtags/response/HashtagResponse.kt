package com.github.nothingelsematters.hashtags.response

import com.beust.klaxon.Json

data class HashtagResponse(val items: List<*>, val count: Int, @Json(name = "total_count") val totalCount: Int)
