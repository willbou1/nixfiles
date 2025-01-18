{
  # TODO: Remember to keep this up to date
  home.file.".local/share/qutebrowser/greasemonkey/youtube-adblock.user.js".text = ''
    // ==UserScript==
    // @name Skip YouTube ads
    // @description Skips the ads in YouTube videos
    // @run-at document-start
    // @include *.youtube.com/*
    // ==/UserScript==

    document.addEventListener('load', () => {
        //if (window.location.toString() === 'https://www.youtube.com/') {
        //    const recommendations = document.getElementById("contents")
        //    if(recommendations) {
        //        recommendations.remove()
        //    }
        //}

        const btn = document.querySelector('.videoAdUiSkipButton,.ytp-ad-skip-button-modern')
        if (btn) {
            btn.click()
        }
        const ad = [...document.querySelectorAll('.ad-showing')][0];
        if (ad) {
            document.querySelector('video').currentTime = 9999999999;
            //document.querySelector('video').currentTime = 0;
        }
    }, true);
  '';
  home.file.".local/share/qutebrowser/greasemonkey/chatgpt.js".text = ''
    // ==UserScript==
    // @name         ChatGPT Auto Stay logged out
    // @namespace    http://tampermonkey.net/
    // @version      2024-12-08
    // @description  Automatically click "Stay logged out" in ChatGPT login reminder popup
    // @author       You
    // @match        https://chatgpt.com/*
    // @icon         https://www.google.com/s2/favicons?sz=64&domain=chatgpt.com
    // @grant        none
    // ==/UserScript==

    (function() {
        'use strict';

        const observer = new MutationObserver((mutationsList, observer) => {
          const stayLoggedOutButton = document.querySelector('[id^="radix-\\:"] > div > div > a');
          if (stayLoggedOutButton) {
            console.log("CHATGPT AUTO STAY: EXECUTED");
            stayLoggedOutButton.click();
            observer.disconnect(); // Stop observing once the element is found and clicked
          }
        });

        observer.observe(document.body, { childList: true, subtree: true });
    })();
  '';
}
