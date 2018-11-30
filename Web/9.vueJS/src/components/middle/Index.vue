<template>
    <div>
        <IndexPost v-for="post in viewPosts" :post="post" :users="users" :key="post.id"/>
    </div>
</template>

<script>
    import IndexPost from "./post/IndexPost";

    export default {
        name: "Index",
        props: ['users', 'posts'],
        computed: {
          viewPosts: function() {
              return Object.values(this.posts).sort((a, b) => b.id - a.id);
          }
        },
        components: {
            IndexPost
        }
    }
</script>

<style scoped>

</style>
