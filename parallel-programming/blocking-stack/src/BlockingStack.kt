interface BlockingStack<E> {
    fun push(element: E)
    suspend fun pop(): E
}