package me.excel.tools.validator.template.workbook;


import me.excel.tools.model.excel.ExcelWorkbook;
import me.excel.tools.model.message.TemplateValidateMessage;

/**
 * sheet size validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class SheetSizeValidator implements WorkbookTemplateValidator {

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
  public TemplateValidateMessage getErrorMessage() {
    return new TemplateValidateMessage(errorMessage);
  }

  @Override
  public boolean validate(ExcelWorkbook workbook) {
    return workbook.sizeOfSheets() == size;
  }
}
