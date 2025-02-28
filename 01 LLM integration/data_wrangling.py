import sentimental.llm as llm
import sentimental.data as data


if __name__ == "__main__":
    from dotenv import load_dotenv

    # Load the NVIDIA API key from the .env file.
    load_dotenv()

    # Go through all the politicians sequentially.
    politicians = {
        "AR": "Albert Rösti",
        "EH": "Eva Herzog",
        "BHK": "Brigitte Häberli-Koller",
        "LM": "Lisa Mazzone",
        "LS": "Lilian Studer",
        "VA": "Viola Amherd",
        "KKS": "Karin Keller-Sutter",
        "EBS": "Elisabeth Baume-Schneider",
        "MR": "Maja Riniker",
        "MM": "Mattea Meyer",
        "IK": "Irène Kälin",
        "AC": "Andrea Caroni",
        "TB": "Thierry Burkart",
        "GP": "Guy Parmelin",
        "IC": "Ignazio Cassis",
        "EN": "Eric Nussbaumer",
        "TA": "Thomas Aeschi",
        "GPF": "Gerhard Pfister",
        "MC": "Martin Candinas",
        "JG": "Jürg Grossen",
    }

    for sheet_name, politician_name in politicians.items():
        print(f"Processing {politician_name}")
        dataframe = data.load_data("sentiment_analysis.xlsx", sheet_name=sheet_name)

        # Get all the lines of text from the dataframe separated by newlines.
        text = dataframe["Content"].str.cat(sep="\n")

        # Analyse the sentiment with the LLM and put that into the "Sentiment" column.
        print("Analyzing sentiment with LLM...")
        dataframe["Sentiment"] = llm.analyse_text_with_llm(
            text, politician=politician_name
        )

        # Save the dataframe to a CSV file.
        dataframe.to_csv(f"{sheet_name}.csv", index=False)
