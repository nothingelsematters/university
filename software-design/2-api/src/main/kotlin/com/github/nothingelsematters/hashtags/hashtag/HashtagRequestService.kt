package com.github.nothingelsematters.hashtags.hashtag

interface HashtagRequestService {

    /**
     * @param hashtag Hashtag to look for. It must be a valid hashtag string (no spaces, no prohibited symbols.
     * @param hourAmount Amount of hours to search in . It must be in `[1, 24]`.
     *
     * @return A list of post amounts for each in [hourAmount] recent hours.
     */
    fun hashtagRecentActivity(hashtag: String, hourAmount: Int): List<Int>
}
