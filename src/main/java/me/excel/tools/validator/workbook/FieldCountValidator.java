package me.excel.tools.validator.workbook;


import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelWorkbook;

/**
 * Created by hanwen on 4/26/16.
 */
public class FieldCountValidator implements WorkbookValidator {

  protected int fieldCount;

  public FieldCountValidator(int fieldCount) {
    this.fieldCount = fieldCount;
  }

  @Override
  public String getErrorMessage() {
    return "字段数量小于" + fieldCount;
  }

  @Override
  public ExcelCell getMessageOnCell(ExcelWorkbook excelWorkbook) {
    return excelWorkbook.getFirstSheet().getRow(0).getCell(0);
  }

  @Override
  public boolean validate(ExcelWorkbook excelWorkbook) {
    return excelWorkbook.getFirstSheet().getKeyRowFields().size() >= fieldCount;
  }
}
