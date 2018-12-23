<template>
    <section>
        <div class="header">
            {{post.title}}
        </div>
        <div class="body">
            {{post.text}}
        </div>
        <div class="footer">
            By {{post.user.login}}
            <a :href="'#page=Post' + post.id" @click="postRedirect(post.id)">View all</a>
        </div>
    </section>

</template>

<script>
    export default {
        props: ['post', 'user'],
        name: "SidebarPost",
        methods: {
            postRedirect: function(postId) {
                this.$root.$emit("onChangePage", 'Post' + postId);
            }
        }
    }
</script>

<style scoped>

</style>
