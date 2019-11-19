#subscription_key = "76fb35d188654aafaa925b1b56f22642"
#endpoint = "https://msft-vision.cognitiveservices.azure.com/"
#vision_base_url = endpoint + "vision/v2.0/"
#text_recognition_url = vision_base_url + "recognizeText"
# Set image_path to the local path of an image that you want to analyze.
#image_path = "C:/Users/allen/Documents/GitHub/alvision/inst/data/cropped/6.png"
#%%
import requests
import time

def azure_post(subscription_key, endpoint, image_path):
    endpoint = "https://msft-vision.cognitiveservices.azure.com/"
    vision_base_url = endpoint + "vision/v2.0/"
    text_recognition_url = vision_base_url + "recognizeText"
    # Read the image into a byte array
    image_data = open(image_path, "rb").read()
    headers = {'Ocp-Apim-Subscription-Key': subscription_key,
               'Content-Type': 'application/octet-stream'}
    params   = {'mode' : 'Handwritten'}
    response = requests.post(
            text_recognition_url, headers=headers, params = params, data=image_data)
    response.raise_for_status()
    return (response, headers)

def azure_get(response, headers):
    # The recognized text isn't immediately available, so poll to wait for completion.
    analysis = {}
    poll = True
    try_idx = 0
    while (poll):
        response_final = requests.get(
              response.headers["Operation-Location"], headers=headers)
        analysis = response_final.json()
        #time.sleep(1)
        if ("recognitionResult" in analysis):
            poll = False
        if ("status" in analysis and analysis['status'] == 'Failed'):
            poll = False
        try_idx += 1
        if (try_idx >= 5):
            poll = False
    return(analysis)

def azure_vis(subscription_key, endpoint, image_path):
    response, headers = azure_post(subscription_key, endpoint, image_path)
    return azure_get(response, headers)
