<template>
    <article>
        <a class="title" :href="'#page=Post' + post.id" @click="postRedirect(post.id)">
            {{ post.title }}
        </a>
        <div class="information">
            By {{ users[post.userId].login }}
        </div>
        <div class="body">
            {{ post.text }}
        </div>
        <div class="footer">
            <div class="left">
                <img src="../../../assets/img/voteup.png" title="Vote Up" alt="Vote Up"/>
                <span class="positive-score">+173</span>
                <img src="../../../assets/img/votedown.png" title="Vote Down" alt="Vote Down"/>
            </div>
            <div class="right">
                <img src="../../../assets/img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                2 days ago
                <img src="../../../assets/img/comments_16x16.png" title="Comments" alt="Comments"/>
                <a href="#">68</a>
            </div>
        </div>
    </article>
</template>

<script>
    export default {
        props: ['users', 'post'],
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
