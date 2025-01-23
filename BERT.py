from transformers import pipeline

pipe = pipeline("text-classification", model="oliverguhr/german-sentiment-bert")
