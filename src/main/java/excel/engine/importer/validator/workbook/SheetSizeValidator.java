package excel.engine.importer.validator.workbook;


import excel.engine.model.excel.Workbook;

/**
 * sheet size validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class SheetSizeValidator implements WorkbookValidator {

  private int size;

  private String errorMessage;

  public SheetSizeValidator(int size) {
    this.size = size;
    this.errorMessage = "工作表数量不是" + size + "个";
  }

  public SheetSizeValidator(String errorMessage, int size) {
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
