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
	"strconv"

	_ "embed"
)

const AOC = "https://adventofcode.com"
const YEAR = "2023"

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

func WriteInput(day int, dir, session string) error {
	resp, err := aocGetRequest(path.Join(YEAR, "day", strconv.Itoa(day), "input"), session)
	if err != nil {
		return err
	}
	defer resp.Body.Close()

	// Dump to input.txt
	input, err := os.Create(path.Join(dir, "input.txt"))
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
	day, err := strconv.ParseInt(os.Args[1], 10, 64)
	if err != nil {
		fmt.Printf("Not a valid day %q: %s\n", os.Args[1], err)
		os.Exit(1)
	}

	session := os.Getenv("AOC_SESSION_TOKEN")
	if session == "" {
		fmt.Println("Set AOC_SESSION_TOKEN in env.")
		os.Exit(1)
	}

	// New directory
	dir := fmt.Sprintf("%02d", day)
	if err := os.Mkdir(dir, 0755); err != nil {
		log.Fatal(err)
	}

	// Template .go file
	err = WriteSolutionTemplate(path.Join(dir, "solve_test.go"))
	if err != nil {
		log.Fatal(err)
	}

	// Get the input
	err = WriteInput(int(day), dir, session)
	if err != nil {
		log.Fatal(err)
	}
}
