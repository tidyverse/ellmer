# Prompt design

This vignette gives you some advice about how to use ellmer to write
prompts. We’ll work through two hopefully relevant examples: a prompt
that generates code and another that extracts structured data. If you’ve
never written a prompt, I’d highly recommend reading Ethan Mollick’s
[Getting started with AI: Good enough
prompting](https://www.oneusefulthing.org/p/getting-started-with-ai-good-enough).
I think understanding his analogy about how AI works will really help
you get started:

> Treat AI like an infinitely patient new coworker who forgets
> everything you tell them each new conversation, one that comes highly
> recommended but whose actual abilities are not that clear. … Two parts
> of this are analogous to working with humans (being new on the job and
> being a coworker) and two of them are very alien (forgetting
> everything and being infinitely patient). We should start with where
> AIs are closest to humans, because that is the key to good-enough
> prompting

As well as learning general prompt design skills, it’s also a good idea
to read any specific advice for the model that you’re using. Here are
some pointers to the prompt design guides of some of the most popular
models:

- [Claude](https://docs.claude.com/en/docs/build-with-claude/prompt-engineering/overview)
- [OpenAI](https://developers.openai.com/api/docs/guides/prompt-engineering)
- [Gemini](https://ai.google.dev/gemini-api/docs/prompting-intro)

If you have a claude account, you can use its
[prompt-generator](https://docs.claude.com/en/docs/build-with-claude/prompt-engineering/prompt-generator).
It’s specifically tailored for Claude, but I suspect it will help you
with many other LLMs, or at least give you some ideas as to what else to
include in your prompt.

``` r

library(ellmer)
```

## Best practices

It’s highly likely that you’ll end up writing long, possibly multi-page
prompts. To ensure your success with this task, we have two
recommendations. First, put each prompt its own, separate file. Second,
write the prompts using markdown. The reason to use markdown is that
it’s quite readable to LLMs (and humans), and it allows you to do things
like use headers to divide up a prompt into sections and itemised lists
to enumerate multiple options. You can see some examples of this style
of prompt here:

- <https://github.com/posit-dev/shiny-assistant/blob/main/shinyapp/app_prompt_python.md>
- <https://github.com/jcheng5/py-sidebot/blob/main/prompt.md>
- <https://github.com/simonpcouch/chores/tree/main/inst/prompts>
- <https://github.com/cpsievert/aidea/blob/main/inst/app/prompt.md>

In terms of file names, if you only have one prompt in your project,
call it `prompt.md`. If you have multiple prompts, give them informative
names like `prompt-extract-metadata.md` or `prompt-summarize-text.md`.
If you’re writing a package, put your prompt(s) in `inst/prompts`,
otherwise it’s fine to put them in the project’s root directory.

Your prompts are going to change over time, so we’d highly recommend
commiting them to a git repo. That will ensure that you can easily see
what has changed, and that if you accidentally make a mistake you can
easily roll back to a known good verison.

If your prompt includes dynamic data, use
[`ellmer::interpolate_file()`](https://ellmer.tidyverse.org/reference/interpolate.md)
to intergrate it into your prompt.
[`interpolate_file()`](https://ellmer.tidyverse.org/reference/interpolate.md)
works like [glue](https://glue.tidyverse.org) but uses `{{ }}` instead
of [`{ }`](https://rdrr.io/r/base/Paren.html) to make it easier to work
with JSON.

As you iterate the prompt, it’s a good idea to build up a small set of
challenging examples that you can regularly re-check with your latest
version of the prompt. Currently you’ll need to do this by hand, but we
hope to eventually provide tools that’ll help you do this a little more
formally.

Unfortunately, you won’t see these best practices in action in this
vignette since we’re keeping the prompts short and inline to make it
easier for you to grok what’s going on.

## Code generation

Let’s explore prompt design for a simple code generation task:

``` r

question <- "
  How can I compute the mean and median of variables a, b, c, and so on,
  all the way up to z, grouped by age and sex.
"
```

I’ll use
[`chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.md)
for this problem because in our experience it does the best job of
generating code.

### Basic flavour

When I don’t provide a system prompt, I sometimes get answers in
different languages or different styles of R code:

``` r

chat <- chat_anthropic()
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> # Computing Mean and Median for Multiple Variables Grouped by Age and 
    #> Sex
    #> 
    #> Here are several approaches depending on your programming 
    #> language/tool:
    #> 
    #> ## R (using dplyr)
    #> 
    #> ```r
    #> library(dplyr)
    #> 
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(across(a:z, list(mean = ~mean(., na.rm = TRUE), 
    #>                                median = ~median(., na.rm = TRUE))))
    #> ```
    #> 
    #> ## Python (using pandas)
    #> 
    #> ```python
    #> import pandas as pd
    #> 
    #> # Assuming your columns a-z are literally named 'a' through 'z'
    #> cols = list('abcdefghijklmnopqrstuvwxyz')
    #> 
    #> result = df.groupby(['age', 'sex'])[cols].agg(['mean', 'median'])
    #> ```
    #> 
    #> ## SQL
    #> 
    #> SQL doesn't have a shorthand for "all columns a-z," so you'd need to 
    #> write it explicitly (or generate the query dynamically):
    #> 
    #> ```sql
    #> SELECT 
    #>     age, 
    #>     sex,
    #>     AVG(a) AS a_mean, 
    #>     -- SQL lacks a native MEDIAN function in many dialects; 
    #>     -- Postgres example using percentile_cont:
    #>     PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY a) AS a_median,
    #>     AVG(b) AS b_mean,
    #>     PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY b) AS b_median
    #>     -- ... continue for c through z
    #> FROM your_table
    #> GROUP BY age, sex;
    #> ```
    #> 
    #> For SQL, it's often easier to **generate this query programmatically**
    #> using Python/R string manipulation if you have 26 variables.
    #> 
    #> ---
    #> 
    #> ### A few clarifying questions to give you a more precise answer:
    #> 
    #> 1. **What tool/language are you using?** (R, Python, SQL, Stata, SPSS,
    #> Excel, etc.)
    #> 2. **Are your variables literally named `a`, `b`, `c`, ... `z`**, or 
    #> is this a placeholder for differently-named variables (e.g., `var1`, 
    #> `income`, `height`, etc.)?
    #> 3. **Do you want the output in long format** (one row per 
    #> variable/stat/group) **or wide format** (one row per group, with 
    #> separate columns for each variable's mean/median)?
    #> 4. **How should missing values (NA) be handled** — excluded from 
    #> calculations, or should they affect the result some other way?
    #> 
    #> Let me know these details and I can tailor the exact code for your 
    #> situation!

I can ensure that I always get R code in a specific style by providing a
system prompt:

``` r

chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers the tidyverse.
"
)
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> Here's how to compute the mean and median across variables `a` through
    #> `z`, grouped by `age` and `sex`:
    #> 
    #> ```r
    #> library(tidyverse)
    #> 
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(
    #>     across(a:z, list(mean = mean, median = median), na.rm = TRUE),
    #>     .groups = "drop"
    #>   )
    #> ```
    #> 
    #> ### Notes:
    #> 
    #> - **`a:z`** uses tidy-select's column range syntax — it works as long 
    #> as your columns are actually named `a`, `b`, `c`, ..., `z` and appear 
    #> in that order in your data frame. This is often more convenient than 
    #> typing out `all_of(letters)`.
    #> 
    #> - If your columns aren't contiguous or ordered alphabetically, use:
    #>   ```r
    #>   across(all_of(letters), list(mean = mean, median = median), na.rm = 
    #> TRUE)
    #>   ```
    #>   since `letters` is a built-in R constant containing `"a"` through 
    #> `"z"`.
    #> 
    #> - **`na.rm = TRUE`** ensures missing values don't cause `NA` results —
    #> remove it if you want NAs to propagate.
    #> 
    #> - The output column names will look like `a_mean`, `a_median`, 
    #> `b_mean`, `b_median`, etc.
    #> 
    #> ### Customizing names
    #> 
    #> If you want a different naming pattern, use the `.names` argument:
    #> 
    #> ```r
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(
    #>     across(
    #>       all_of(letters),
    #>       list(mean = mean, median = median),
    #>       na.rm = TRUE,
    #>       .names = "{.fn}_{.col}"
    #>     ),
    #>     .groups = "drop"
    #>   )
    #> ```
    #> 
    #> This would produce names like `mean_a`, `median_a`, `mean_b`, 
    #> `median_b`, etc.

Note that I’m using both a system prompt (which defines the general
behaviour) and a user prompt (which asks the specific question). You
could put all this content in the user prompt and get similar results,
but I think it’s helpful to use both to cleanly divide the general
framing of the response from the specific questions you ask.

Since I’m mostly interested in the code, I ask it to drop the
explanation and sample data:

``` r

chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers the tidyverse.
  Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> ```r
    #> df %>%
    #>   group_by(age, sex) %>%
    #>   summarise(across(a:z, list(mean = mean, median = median), na.rm = 
    #> TRUE, .names = "{.col}_{.fn}"))
    #> ```

And of course, if you want a different style of R code, just ask for it:

``` r

chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers data.table.
  Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> ```r
    #> dt[, lapply(.SD, function(x) list(mean = mean(x), median = 
    #> median(x))),
    #>    by = .(age, sex), .SDcols = letters]
    #> ```
    #> 
    #> If you want mean and median as separate columns instead of a list 
    #> column:
    #> 
    #> ```r
    #> cols <- letters
    #> dt[, unlist(lapply(.SD, function(x) list(mean = mean(x), median = 
    #> median(x))), recursive = FALSE),
    #>    by = .(age, sex), .SDcols = cols]
    #> ```

``` r


chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers base R.
  Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> ```r
    #> vars <- letters
    #> result <- aggregate(. ~ age + sex, data = df[c("age", "sex", vars)],
    #>                      FUN = function(x) c(mean = mean(x), median = 
    #> median(x)))
    #> ```

### Be explicit

If there’s something about the output that you don’t like, try being
more explicit. For example, the code isn’t styled quite how I’d like it,
so I provide more details about what I do want:

``` r

chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer who prefers the tidyverse.
  Just give me the code. I don't want any explanation or sample data.

  Follow the tidyverse style guide:
  * Spread long function calls across multiple lines.
  * Where needed, always indent function calls with two spaces.
  * Only name arguments that are less commonly used.
  * Always use double quotes for strings.
  * Use the base pipe, `|>`, not the magrittr pipe `%>%`.
"
)
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> ```r
    #> df |>
    #>   summarise(
    #>     across(a:z, list(mean = mean, median = median), na.rm = TRUE),
    #>     .by = c(age, sex)
    #>   )
    #> ```

This still doesn’t yield exactly the code that I’d write, but it’s
pretty close.

You could provide a different prompt if you were looking for more
explanation of the code:

``` r

chat <- chat_anthropic(
  system_prompt = "
  You are an expert R teacher.
  I am a new R user who wants to improve my programming skills.
  Help me understand the code you produce by explaining each function call with
  a brief comment. For more complicated calls, add documentation to each
  argument. Just give me the code. I don't want any explanation or sample data.
"
)
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> ```r
    #> library(dplyr) # load dplyr for data manipulation (group_by, 
    #> summarise, across)
    #> 
    #> result <- df %>%                      # start pipeline with data frame
    #> 'df'
    #>   group_by(age, sex) %>%               # group rows by combinations of
    #> 'age' and 'sex'
    #>   summarise(
    #>     across(
    #>       .cols = letters,                 # 'letters' is R's built-in 
    #> vector c("a","b",...,"z")
    #>                                         # selects all columns named a 
    #> through z
    #>       .fns = list(
    #>         mean   = ~ mean(.x, na.rm = TRUE),   # compute mean of each 
    #> column, ignoring NAs
    #>         median = ~ median(.x, na.rm = TRUE)  # compute median of each 
    #> column, ignoring NAs
    #>       ),
    #>       .names = "{.col}_{.fn}"          # name output columns like 
    #> "a_mean", "a_median", etc.
    #>     ),
    #>     .groups = "drop"                   # drop grouping structure after
    #> summarising
    #>   )
    #> 
    #> print(result) # display the resulting summary table
    #> ```

### Teach it about new features

You can imagine LLMs as being a sort of an average of the internet at a
given point in time. That means they will provide popular answers, which
will tend to reflect older coding styles (either because the new
features aren’t in their index, or the older features are so much more
popular). So if you want your code to use specific newer language
features, you might need to provide the examples yourself:

``` r

chat <- chat_anthropic(
  system_prompt = "
  You are an expert R programmer.
  Just give me the code; no explanation in text.
  Use the `.by` argument rather than `group_by()`.
  dplyr 1.1.0 introduced per-operation grouping with the `.by` argument.
  e.g., instead of:

  transactions |>
    group_by(company, year) |>
    mutate(total = sum(revenue))

  write this:
  transactions |>
    mutate(
      total = sum(revenue),
      .by = c(company, year)
    )
"
)
#> Using model = "claude-sonnet-5".
chat$chat(question)
```

    #> ```r
    #> df |>
    #>   summarise(
    #>     across(a:z, list(mean = mean, median = median)),
    #>     .by = c(age, sex)
    #>   )
    #> ```

## Structured data

Providing a rich set of examples is a great way to encourage the output
to produce exactly what you want. This is known as **multi-shot
prompting**. Below we’ll work through a prompt that I designed to
extract structured data from recipes, but the same ideas apply in many
other situations.

### Getting started

My overall goal is to turn a list of ingredients, like the following,
into a nicely structured JSON that I can then analyse in R (e.g. compute
the total weight, scale the recipe up or down, or convert the units from
volumes to weights).

``` r

ingredients <- "
  ¾ cup (150g) dark brown sugar
  2 large eggs
  ¾ cup (165g) sour cream
  ½ cup (113g) unsalted butter, melted
  1 teaspoon vanilla extract
  ¾ teaspoon kosher salt
  ⅓ cup (80ml) neutral oil
  1½ cups (190g) all-purpose flour
  150g plus 1½ teaspoons sugar
"
```

(This isn’t the ingredient list for a real recipe but it includes a
sampling of styles that I encountered in my project.)

If you don’t have strong feelings about what the data structure should
look like, you can start with a very loose prompt and see what you get
back. I find this a useful pattern for underspecified problems where the
heavy lifting lies with precisely defining the problem you want to
solve. Seeing the LLM’s attempt to create a data structure gives me
something to react to, rather than having to start from a blank page.

``` r

instruct_json <- "
  You're an expert baker who also loves JSON. I am going to give you a list of
  ingredients and your job is to return nicely structured JSON. Just return the
  JSON and no other commentary.
"

chat <- chat_openai(instruct_json)
#> Using model = "gpt-5.6-terra".
chat$chat(ingredients)
#> [
#>   {
#>     "ingredient": "dark brown sugar",
#>     "quantity": {
#>       "cups": "3/4",
#>       "grams": 150
#>     }
#>   },
#>   {
#>     "ingredient": "eggs",
#>     "quantity": 2,
#>     "unit": "large"
#>   },
#>   {
#>     "ingredient": "sour cream",
#>     "quantity": {
#>       "cups": "3/4",
#>       "grams": 165
#>     }
#>   },
#>   {
#>     "ingredient": "unsalted butter",
#>     "quantity": {
#>       "cups": "1/2",
#>       "grams": 113
#>     },
#>     "preparation": "melted"
#>   },
#>   {
#>     "ingredient": "vanilla extract",
#>     "quantity": 1,
#>     "unit": "teaspoon"
#>   },
#>   {
#>     "ingredient": "kosher salt",
#>     "quantity": "3/4",
#>     "unit": "teaspoon"
#>   },
#>   {
#>     "ingredient": "neutral oil",
#>     "quantity": {
#>       "cups": "1/3",
#>       "milliliters": 80
#>     }
#>   },
#>   {
#>     "ingredient": "all-purpose flour",
#>     "quantity": {
#>       "cups": "1 1/2",
#>       "grams": 190
#>     }
#>   },
#>   {
#>     "ingredient": "sugar",
#>     "quantity": {
#>       "grams": 150,
#>       "teaspoons": "1 1/2"
#>     }
#>   }
#> ]
```

(I don’t know if the additional colour, “You’re an expert baker who also
loves JSON”, does anything, but I like to think this helps the LLM get
into the right mindset of a very nerdy baker.)

### Provide examples

This isn’t a bad start, but I prefer to cook with weight and I only want
to see volumes if weight isn’t available so I provide a couple of
examples of what I’m looking for. I was pleasantly suprised that I can
provide the input and output examples in such a loose format.

``` r

instruct_weight <- r"(
  Here are some examples of the sort of output I'm looking for:

  ¾ cup (150g) dark brown sugar
  {"name": "dark brown sugar", "quantity": 150, "unit": "g"}

  ⅓ cup (80ml) neutral oil
  {"name": "neutral oil", "quantity": 80, "unit": "ml"}

  2 t ground cinnamon
  {"name": "ground cinnamon", "quantity": 2, "unit": "teaspoon"}
)"

chat <- chat_openai(paste(instruct_json, instruct_weight))
#> Using model = "gpt-5.6-terra".
chat$chat(ingredients)
#> [
#>   {
#>     "name": "dark brown sugar",
#>     "quantity": 150,
#>     "unit": "g"
#>   },
#>   {
#>     "name": "eggs",
#>     "quantity": 2,
#>     "unit": "large"
#>   },
#>   {
#>     "name": "sour cream",
#>     "quantity": 165,
#>     "unit": "g"
#>   },
#>   {
#>     "name": "unsalted butter, melted",
#>     "quantity": 113,
#>     "unit": "g"
#>   },
#>   {
#>     "name": "vanilla extract",
#>     "quantity": 1,
#>     "unit": "teaspoon"
#>   },
#>   {
#>     "name": "kosher salt",
#>     "quantity": 0.75,
#>     "unit": "teaspoon"
#>   },
#>   {
#>     "name": "neutral oil",
#>     "quantity": 80,
#>     "unit": "ml"
#>   },
#>   {
#>     "name": "all-purpose flour",
#>     "quantity": 190,
#>     "unit": "g"
#>   },
#>   {
#>     "name": "sugar",
#>     "quantity": 150,
#>     "unit": "g",
#>     "additional_quantity": 1.5,
#>     "additional_unit": "teaspoon"
#>   }
#> ]
```

Just providing the examples seems to work remarkably well. But I found
it useful to also include a description of what the examples are trying
to accomplish. I’m not sure if this helps the LLM or not, but it
certainly makes it easier for me to understand the organisation of the
whole prompt and check that I’ve covered the key pieces I’m interested
in.

``` r

instruct_weight <- r"(
  * If an ingredient has both weight and volume, extract only the weight:

  ¾ cup (150g) dark brown sugar
  [
    {"name": "dark brown sugar", "quantity": 150, "unit": "g"}
  ]

* If an ingredient only lists a volume, extract that.

  2 t ground cinnamon
  ⅓ cup (80ml) neutral oil
  [
    {"name": "ground cinnamon", "quantity": 2, "unit": "teaspoon"},
    {"name": "neutral oil", "quantity": 80, "unit": "ml"}
  ]
)"
```

This structure also allows me to give the LLMs a hint about how I want
multiple ingredients to be stored, i.e. as an JSON array.

I then iterated on the prompt, looking at the results from different
recipes to get a sense of what the LLM was getting wrong. Much of this
felt like I waws iterating on my own understanding of the problem as I
didn’t start by knowing exactly how I wanted the data. For example, when
I started out I didn’t really think about all the various ways that
ingredients are specified. For later analysis, I always want quantities
to be number, even if they were originally fractions, or the if the
units aren’t precise (like a pinch). It made me realise that some
ingredients are unitless.

``` r

instruct_unit <- r"(
* If the unit uses a fraction, convert it to a decimal.

  ⅓ cup sugar
  ½ teaspoon salt
  [
    {"name": "dark brown sugar", "quantity": 0.33, "unit": "cup"},
    {"name": "salt", "quantity": 0.5, "unit": "teaspoon"}
  ]

* Quantities are always numbers

  pinch of kosher salt
  [
    {"name": "kosher salt", "quantity": 1, "unit": "pinch"}
  ]

* Some ingredients don't have a unit.
  2 eggs
  1 lime
  1 apple
  [
    {"name": "egg", "quantity": 2},
    {"name": "lime", "quantity": 1},
    {"name", "apple", "quantity": 1}
  ]
)"
```

You might want to take a look at the [full
prompt](https://gist.github.com/hadley/7688b4dd1e5e97b800c6d7d79e437b48)
to see what I ended up with.

### Structured data

Now that I’ve iterated to get a data structure I like, it seems useful
to formalise it and tell the LLM exactly what I’m looking for when
dealing with structured data. This guarantees that the LLM will only
return JSON, that the JSON will have the fields that you expect, and
that ellmer will convert it into an R data structure.

``` r

type_ingredient <- type_object(
  name = type_string("Ingredient name"),
  quantity = type_number(),
  unit = type_string("Unit of measurement")
)

type_ingredients <- type_array(type_ingredient)

chat <- chat_openai(c(instruct_json, instruct_weight))
#> Using model = "gpt-5.6-terra".
chat$chat_structured(ingredients, type = type_ingredients)
#> # A tibble: 10 × 3
#>    name                    quantity unit      
#>    <chr>                      <dbl> <chr>     
#>  1 dark brown sugar          150    "g"       
#>  2 large eggs                  2    ""        
#>  3 sour cream                165    "g"       
#>  4 unsalted butter, melted   113    "g"       
#>  5 vanilla extract             1    "teaspoon"
#>  6 kosher salt                 0.75 "teaspoon"
#>  7 neutral oil                80    "ml"      
#>  8 all-purpose flour         190    "g"       
#>  9 sugar                     150    "g"       
#> 10 sugar                       1.5  "teaspoon"
```

### Capturing raw input

One thing that I’d do next time would also be to include the raw
ingredient names in the output. This doesn’t make much difference in
this simple example but it makes it much easier to align the input with
the output and to start developing automated measures of how well my
prompt is doing.

``` r

instruct_weight_input <- r"(
  * If an ingredient has both weight and volume, extract only the weight:

    ¾ cup (150g) dark brown sugar
    [
      {"name": "dark brown sugar", "quantity": 150, "unit": "g", "input": "¾ cup (150g) dark brown sugar"}
    ]

  * If an ingredient only lists a volume, extract that.

    2 t ground cinnamon
    ⅓ cup (80ml) neutral oil
    [
      {"name": "ground cinnamon", "quantity": 2, "unit": "teaspoon", "input": "2 t ground cinnamon"},
      {"name": "neutral oil", "quantity": 80, "unit": "ml", "input": "⅓ cup (80ml) neutral oil"}
    ]
)"
```

I think this is particularly important if you’re working with even less
structured text. For example, imagine you had this text:

``` r

recipe <- r"(
  In a large bowl, cream together one cup of softened unsalted butter and a
  quarter cup of white sugar until smooth. Beat in an egg and 1 teaspoon of
  vanilla extract. Gradually stir in 2 cups of all-purpose flour until the
  dough forms. Finally, fold in 1 cup of semisweet chocolate chips. Drop
  spoonfuls of dough onto an ungreased baking sheet and bake at 350°F (175°C)
  for 10-12 minutes, or until the edges are lightly browned. Let the cookies
  cool on the baking sheet for a few minutes before transferring to a wire
  rack to cool completely. Enjoy!
)"
```

Including the input text in the output makes it easier to see if it’s
doing a good job:

``` r

chat <- chat_openai(c(instruct_json, instruct_weight_input))
#> Using model = "gpt-5.6-terra".
chat$chat(recipe)
#> [
#>   {
#>     "name": "unsalted butter, softened",
#>     "quantity": 1,
#>     "unit": "cup",
#>     "input": "one cup of softened unsalted butter"
#>   },
#>   {
#>     "name": "white sugar",
#>     "quantity": 0.25,
#>     "unit": "cup",
#>     "input": "a quarter cup of white sugar"
#>   },
#>   {
#>     "name": "egg",
#>     "quantity": 1,
#>     "unit": "egg",
#>     "input": "an egg"
#>   },
#>   {
#>     "name": "vanilla extract",
#>     "quantity": 1,
#>     "unit": "teaspoon",
#>     "input": "1 teaspoon of vanilla extract"
#>   },
#>   {
#>     "name": "all-purpose flour",
#>     "quantity": 2,
#>     "unit": "cup",
#>     "input": "2 cups of all-purpose flour"
#>   },
#>   {
#>     "name": "semisweet chocolate chips",
#>     "quantity": 1,
#>     "unit": "cup",
#>     "input": "1 cup of semisweet chocolate chips"
#>   }
#> ]
```

When I ran it while writing this vignette, it seemed to be working out
the weight of the ingredients specified in volume, even though the
prompt specifically asks it not to. This may suggest I need to broaden
my examples.

## Token usage

| provider  | model           | input | output | cached_input |  price |
|:----------|:----------------|------:|-------:|-------------:|-------:|
| Anthropic | claude-sonnet-5 |   964 |   2263 |            0 | \$0.02 |
| OpenAI    | gpt-5.6-terra   |  1119 |   1170 |            0 | \$0.02 |
