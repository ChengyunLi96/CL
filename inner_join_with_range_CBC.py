from typing import List, Tuple, Optional
from datetime import date
from tqdm.auto import tqdm
# Since the files have been sorted by patient ID
# Find all records for one patient
# Once a patient from two files are aligned, search for time range

# Given a file handle, return all records from one patient
def patient_record_iterator(file_handle, desc:str, delimiter:Optional[str] = None) -> Tuple[date, List[Tuple[str, List]]]:
    """Yield all records of the same patient
    
    Args:
        file_handle (file object): The file handle
        desc (str): Description for this iterator
        delimiter (str): Separator defined by this file
    
    Returns:
        Tuple[str, List[Tuple[date, List]], bool]: In the form of (PatientID, [(date, [fields expect for PatientID])] * num_records, has_next)
    """
    current_patient = ""
    records: List[Tuple[date, List]]= []
    for line in tqdm(file_handle, desc=desc):
        if delimiter:
            fields = line.strip().split(delimiter)
        else:
            fields = line.strip().split()
        # Set the initial value
        if not current_patient:
            current_patient = fields[0]
        # If we see a new patient, yield the record for current patient
        if fields[0] != current_patient:
            yield current_patient, records
            # Reset current patient & records
            current_patient = fields[0]
            records = []
        # Process date
        year, month, day = [int(value) for value in fields[1].split("T")[0].split("-")]
        # Add in the current record
        records.append((date(year, month, day), fields[1:]))
    yield current_patient, records


if __name__ == "__main__":
    CBC = ["19023-1", "21000-5", "30229-9", "32623-1", "35332-6", "42250-1", "43743-4", "4544-3", "5905-5", "6690-2", 
           "704-7", "706-2", "711-2", "713-8", "718-7", "731-0", "736-9", "742-7", "751-8", "770-8", "777-3", "785-6",
           "786-4", "787-2", "788-0", "789-8"]
    for CBC_test in CBC:
        with open(CBC_test + "_joined.txt", "w") as joined_file:
            with open(CBC_test + "_sorted.txt") as test_file:
                with open("uniq_rx_pso_skin_sorted.txt") as drug_file:
                    test_record_iterator = patient_record_iterator(test_file, desc="Test file")
                    drug_record_iterator = patient_record_iterator(drug_file, desc="Drug file", delimiter=",")
                    test_patient, test_records = next(test_record_iterator)
                    drug_patient, drug_records = next(drug_record_iterator)
                    # If both files are not over, keep looking
                    # Handle stop iteration
                    try:
                        while test_record_iterator and drug_record_iterator:
                            if drug_patient > test_patient:
                                # Increment test records
                                test_patient, test_records = next(test_record_iterator)
                            elif test_patient > drug_patient:
                                # Increment drug records
                                drug_patient, drug_records = next(drug_record_iterator)
                            else:
                                # When the two aligns, start checking for date, date is also sorted in increasing order
                                for test_date, test_fields in test_records:
                                    for drug_date, drug_fields in drug_records:
                                        # Calculate date time difference
                                        delta = drug_date - test_date
                                        # If over "60" days, all remaining drug records will be longer than "60" days
                                        if delta.days > 60:
                                            break
                                        # Only join when test_date is sooner than drug_date
                                        elif delta.days >= 0:
                                            joined_file.write("\t".join([test_patient] + test_fields + drug_fields))
                                            joined_file.write("\n")
                                test_patient, test_records = next(test_record_iterator)
                                drug_patient, drug_records = next(drug_record_iterator)
                    except StopIteration:
                        print("Finished joining")
                                    

                                



