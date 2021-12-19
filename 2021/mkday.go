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

// aocGetRequest puts in the correct cookies to talk with AoC.
func aocGetRequest(path, session string) (*http.Response, error) {

	url := &url.URL{
		Scheme: "https",
		Host:   "adventofcode.com",
		Path:   path,
	}

	req, err := http.NewRequest(http.MethodGet, url.String(), nil)
	if err != nil {
		return nil, err
	}
	req.AddCookie(&http.Cookie{Name: "session", Value: session})

	jar, err := cookiejar.New(nil)
	if err != nil {
		return nil, err
	}
	client := http.Client{
		Jar: jar,
	}
	return client.Do(req)
}

func WriteInput(day, session string) error {

	resp, err := aocGetRequest(path.Join(YEAR, "day", day, "input"), session)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	// Dump to input.txt
	input, err := os.Create(path.Join(day, "input.txt"))
	if err != nil {
		return err
	}
	_, err = io.Copy(input, resp.Body)
	if err != nil {
		return err
	}
	err = input.Sync()
	if err != nil {
		return err
	}

	return nil
}

func WriteSolutionTemplate(path string) error {
	return os.WriteFile(path, []byte(template), 0644)
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("usage: mkday day")
		os.Exit(1)
	}
	day := os.Args[1]

	session := os.Getenv("AOCSESSION")
	if session == "" {
		fmt.Println("Set AOCSESSION in env.")
		os.Exit(1)
	}

	// New directory
	if err := os.Mkdir(day, 0755); err != nil {
		log.Fatal(err)
	}

	// Template .go file
	err := WriteSolutionTemplate(path.Join(day, "solve_test.go"))
	if err != nil {
		log.Fatal(err)
	}

	// Get the input
	err = WriteInput(day, session)
	if err != nil {
		log.Fatal(err)
	}
}
