## presentation comments

### bmb

I apologize, I might have been running out of steam with my comments: I have fewer notes than I'd like (having your presentation document might help refresh my memory).

- In general `[g]lmer` and `glmmTMB` *should* converge to the same results (for any models that they can both fit); if they seem to be giving different answers, you need to check very carefully what's going on.

- as Dushoff mentioned, you could consider analyzing PC1 (since the fact that PC1 has both positive and negative loadings indicates that there's not a single "size" variable to use for super-vein analysis; in any case, I would consider the sum of vein lengths to be a pretty crude/desperate way to do the analysis)

- be careful about proving that "interactions exist": you want to try to find ways to quantify the **magnitude** of interactions/epistatic effects - this can admittedly be hard for effects that are summarized by multiple parameters
