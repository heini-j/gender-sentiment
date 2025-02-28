import pandas as pd


def load_data(file_path, sheet_name):
    df = pd.read_excel(file_path, sheet_name=sheet_name)
    df = df.melt(["Paper", "Date", "Title of article"], value_name="Content").dropna()
    return df
