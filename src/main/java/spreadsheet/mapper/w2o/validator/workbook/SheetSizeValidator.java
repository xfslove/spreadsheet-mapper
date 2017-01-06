package spreadsheet.mapper.w2o.validator.workbook;


import spreadsheet.mapper.model.core.Workbook;

/**
 * sheet size validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class SheetSizeValidator implements WorkbookValidator {

  private int size;

  private String errorMessage;

  public SheetSizeValidator(int size, String errorMessage) {
    this.size = size;
    this.errorMessage = errorMessage;
  }

  @Override
  public String getErrorMessage() {
    return errorMessage;
  }

  @Override
  public boolean valid(Workbook workbook) {
    return workbook.sizeOfSheets() == size;
  }
}
