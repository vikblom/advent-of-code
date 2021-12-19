package main

import (
	"fmt"
	"io"
	"log"
	"net/http"
	"net/http/cookiejar"
	"net/url"
	"os"
	"path"

	_ "embed"
)

const AOC = "https://adventofcode.com"
const YEAR = "2021"

//go:embed template.txt
var template string

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: mkday day")
		os.Exit(1)
	}
	day := os.Args[1]

	token := os.Getenv("AOCCOOKIE")
	if token == "" {
		fmt.Println("Set AOCCOOKIE in env.")
		os.Exit(1)
	}

	// New directory
	if err := os.Mkdir(day, 0755); err != nil {
		log.Fatal(err)
	}

	// Template .go file
	solve := path.Join(day, "solve_test.go")
	if err := os.WriteFile(solve, []byte(template), 0644); err != nil {
		log.Fatal(err)
	}

	// Request the input.
	cookie := &http.Cookie{
		Name:  "session",
		Value: token,
	}

	url := &url.URL{
		Scheme: "https",
		Host:   "adventofcode.com",
		Path:   path.Join(YEAR, "day", day, "input"),
	}

	req, err := http.NewRequest(http.MethodGet, url.String(), nil)
	if err != nil {
		log.Fatal(err)
	}
	req.AddCookie(cookie)

	jar, err := cookiejar.New(nil)
	if err != nil {
		log.Fatal(err)
	}
	client := http.Client{
		Jar: jar,
	}

	resp, err := client.Do(req)
	if err != nil {
		log.Fatal(err)
	}
	defer resp.Body.Close()

	// Dump to input.txt
	input, err := os.Create(path.Join(day, "input.txt"))
	if err != nil {
		log.Fatal(err)
	}
	_, err = io.Copy(input, resp.Body)
	if err != nil {
		log.Fatal(err)
	}
	err = input.Sync()
	if err != nil {
		log.Fatal(err)
	}
}
