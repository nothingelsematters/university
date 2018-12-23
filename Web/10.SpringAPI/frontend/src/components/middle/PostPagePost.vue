<template>
    <article>
        <div class="title">
            {{ post.title }}
        </div>
        <div class="information">
            By {{ post.user.login }}
        </div>
        <div class="body">
            {{ post.text }}
        </div>
        <div class="footer">
            <div class="left">
                <img src="../../assets/img/voteup.png" title="Vote Up" alt="Vote Up"/>
                <span class="positive-score">+173</span>
                <img src="../../assets/img/votedown.png" title="Vote Down" alt="Vote Down"/>
            </div>
            <div class="right">
                <img src="../../assets/img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                2 days ago
                <img src="../../assets/img/comments_16x16.png" title="Comments" alt="Comments"/>
                <a href="#">68</a>
            </div>
        </div>
        <Comment v-for="comment in post.comments" :comment="comment" :key="comment.id"/>
        <form @submit.prevent="onAdd">
            <div>
                <label for="comment">
                    Comment:
                </label>
                <textarea id="comment" rows="2" v-model="newComment"></textarea>
            </div>
            <div class="error">{{error}}</div>
            <div>
                <input type="submit" value="Add"/>
            </div>
        </form>
    </article>
</template>

<script>
    import Comment from './Comment'
    import axios from 'axios'

    export default {
        props: ['post', 'newComment', 'error', 'comments'],
        name: "PostPagePost",
        components: {
            Comment
        },
        methods: {
            onAdd: function () {
                this.error = "";
                axios.post("comments", {
                    text: this.newComment,
                    noticeId: this.post.id
                }).then(function() {
                    this.$root.$emit("onAddComment");
                }).catch(error => {
                    this.error = error.response.data.message;
                });
            }
        }
    }
</script>

<style scoped>

</style>
