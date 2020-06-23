package mutex

/**
 * Distributed mutual exclusion implementation.
 * All functions are called from the single main thread.
 *
 * @author Simon Naumov
 */
class ProcessImpl(private val env: Environment) : Process {
    val forks = MutableList(env.nProcesses + 1) { ForkType.DIRTY }

    init {
        for (i in env.processId + 1 until env.nProcesses + 1) {
            forks[i] = ForkType.ABSENT
        }
    }

    var lockRequested = false

    var locked = false

    val forkRequested = MutableList(env.nProcesses + 1) { false }

    override fun onMessage(srcId: Int, message: Message) {
        message.parse {
            when (readEnum<MsgType>()) {
                MsgType.FORK -> {
                    forks[srcId] = ForkType.CLEAN
                    tryLock()
                }

                MsgType.REQUEST -> {
                    if (locked || forks[srcId] != ForkType.DIRTY) {
                        forkRequested[srcId] = true
                    } else {
                        forks[srcId] = ForkType.ABSENT
                        forkRequested[srcId] = false

                        if (lockRequested) {
                            send(srcId, MsgType.REQUEST)
                        }
                        send(srcId, MsgType.FORK)
                    }
                }
            }
        }
    }

    override fun onLockRequest() {
        lockRequested = true

        if (!tryLock()) {
            forks.forEachIndexed { index, f ->
                if (f == ForkType.ABSENT) send(index, MsgType.REQUEST)
            }
        }
    }

    private fun tryLock(): Boolean {
        if (lockRequested && forks.all { it != ForkType.ABSENT }) {
            env.locked()
            locked = true
            return true
        }

        return false
    }

    override fun onUnlockRequest() {
        lockRequested = false
        forks.indices.forEach { forks[it] = ForkType.DIRTY }
        env.unlocked()
        locked = false

        forkRequested.indices
            .asSequence()
            .filter { forkRequested[it] }
            .forEach {
                forks[it] = ForkType.ABSENT
                send(it, MsgType.FORK)
                forkRequested[it] = false
            }
    }

    private fun send(destId: Int, type: MsgType) {
        env.send(destId) {
            writeEnum(type)
        }
    }

    enum class ForkType { ABSENT, CLEAN, DIRTY }
    enum class MsgType { REQUEST, FORK }
}
