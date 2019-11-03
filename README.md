# haskellDB

An application aiming to provide a (not so) easy way to manage the works commissioned to Maker Station.

It's a lovely mix of Haskell, HTML, Javascript and CSS.

## How to make it work

### Requirements:
- PostgreSQL 11.5
- GHCI >= 8.6.5
- Stack
- Internet browser of your choice

### Preparation:
- Install the softwares listed above
- Clone this repository
- Use "db_schema.sql" to generate the database (use pgAdmin, or whatever tool you are most comfortable with)
- Create a file named "server.cfg" in the repository directory, containing the following lines:
    ```
    db="name_of_your_db"
    dbLocation="localhost"
    dbPort=5432
    dbUser="your_db_user"
    port=8080
    apiName="MSManager"
    ```
    Changes could be necessary, depending on your postgres installation and necessities.
- Move to the repository directory on the command line
- Use `stack ghci`

The following steps are needed to create the first admin user.

- In the GHCI prompt use `connectWithInfo "localhost" 5432 "your_db_user" "your_db_user_password" "name_of_your_db" >>= insertUser "username" "password"`, changing the function arguments with the ones you need and wish to use.
- Open pgAdmin (or whatever tool you use to execute queries in the database) and execute the query `UPDATE utenti SET admin = true WHERE username = 'username'`, changing username with the one chosen in the previous step.

You have now created your first admin user!

- Return to the GHCI prompt, use `:main PASSWORD`, where PASSWORD is the password of the PostgreSQL user you used in the "server.cfg" file
    - If you wish to execute the server outside of the GHCI prompt (and that would be great!) use Stack to build and install the application, and call it from the command line with PASSWORD as the first argument
- Open your browser, and connect to `localhost:8080`
    - The web interface was tested on Firefox, but it should work on other browsers too (please, avoid Internet Explorer)
- That's it. Log in, and everything should work!

## A little disclaimer
- This system doesn't aim to be safe: at the moment, I don't have the knowledge to make it safe. So, it isn't. Don't use as if it were. In particular, the main problems are:
    - there is no safe connection to the server (no https), so your passwords are sent without encryption
- This is not easy to install, and some operations are doable only through SQL queries: again, I have neither the knowledge nor the time to resolve this
- The user interface is probably badly written, and not accessible: I'm still learning, and I will (probably) correct it in the future
- The Haskell code works, but it could be better: I repeat, I'm still learning