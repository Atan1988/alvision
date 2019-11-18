import requests
import time
# If you are using a Jupyter notebook, uncomment the following line.
# %matplotlib inline
import matplotlib.pyplot as plt
from matplotlib.patches import Polygon
from PIL import Image
from io import BytesIO

# Add your Computer Vision subscription key and endpoint to your environment variables.
# if 'COMPUTER_VISION_SUBSCRIPTION_KEY' in os.environ:
#     subscription_key = os.environ['COMPUTER_VISION_SUBSCRIPTION_KEY']
# else:
#     print("\nSet the COMPUTER_VISION_SUBSCRIPTION_KEY environment variable.\n**Restart your shell or IDE for changes to take effect.**")
#     sys.exit()
#
# if 'COMPUTER_VISION_ENDPOINT' in os.environ:
#     endpoint = os.environ['COMPUTER_VISION_ENDPOINT']

subscription_key = "76fb35d188654aafaa925b1b56f22642"
endpoint = "https://msft-vision.cognitiveservices.azure.com/"
vision_base_url = endpoint + "vision/v2.0/"
#text_recognition_url = endpoint + "vision/v2.0/recognizeText[Handwritten]"
text_recognition_url = vision_base_url + "recognizeText"
#%%
# Set image_path to the local path of an image that you want to analyze.
image_path = "C:/Users/allen/Documents/GitHub/alvision/inst/data/cropped/36b.png"

# Read the image into a byte array
image_data = open(image_path, "rb").read()

headers = {'Ocp-Apim-Subscription-Key': subscription_key,
           'Content-Type': 'application/octet-stream'}
params   = {'mode' : 'Handwritten'}
#params = {'visualFeatures': 'OCR'}
response = requests.post(
    text_recognition_url, headers=headers, params = params, data=image_data)
response.raise_for_status()
#%%
response.content
#%%
# Extracting text requires two API calls: One call to submit the
# image for processing, the other to retrieve the text found in the image.

# Holds the URI used to retrieve the recognized text.
operation_url = response.headers["Operation-Location"]

# The recognized text isn't immediately available, so poll to wait for completion.
analysis = {}
poll = True
#%%
#while (poll):
#%%
    response_final = requests.get(
        response.headers["Operation-Location"], headers=headers)
    analysis = response_final.json()
    print(analysis)
#%%
    ("recognitionResult" in analysis)
#%%
    time.sleep(1)
    if ("recognitionResult" in analysis):
        poll = False
    if ("status" in analysis and analysis['status'] == 'Failed'):
        poll = False

polygons = []
if ("recognitionResult" in analysis):
    # Extract the recognized text, with bounding boxes.
    polygons = [(line["boundingBox"], line["text"])
                for line in analysis["recognitionResult"][0]["lines"]]
#%%
# Display the image and overlay it with the extracted text.
plt.figure(figsize=(15, 15))
image = Image.open(BytesIO(requests.get(image_url).content))
ax = plt.imshow(image)
for polygon in polygons:
    vertices = [(polygon[0][i], polygon[0][i+1])
                for i in range(0, len(polygon[0]), 2)]
    text = polygon[1]
    patch = Polygon(vertices, closed=True, fill=False, linewidth=2, color='y')
    ax.axes.add_patch(patch)
    plt.text(vertices[0][0], vertices[0][1], text, fontsize=20, va="top")
