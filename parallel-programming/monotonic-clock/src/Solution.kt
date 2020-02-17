class Solution : MonotonicClock {
    private var primary1 by RegularInt(0)
    private var primary2 by RegularInt(0)
    private var primary3 by RegularInt(0)

    private var secondary1 by RegularInt(0)
    private var secondary2 by RegularInt(0)
    private var secondary3 by RegularInt(0)

    override fun write(time: Time) {
        secondary1 = time.d1
        secondary2 = time.d2
        secondary3 = time.d3

        // write right-to-left
        primary3 = time.d3
        primary2 = time.d2
        primary1 = time.d1
    }

    override fun read(): Time {
        val first1 = primary1
        val first2 = primary2
        val first3 = primary3

        val second3 = secondary3
        val second2 = secondary2
        val second1 = secondary1

        return if (first1 == second1) {
                if (first2 == second2) {
                    Time(first1, first2, if (first3 == second3) first3 else second3)
                } else {
                    Time(first1, second2, 0)
                }
            } else {
                Time(second1, 0, 0)
            }
    }
}
