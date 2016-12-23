package me.excel.tools.validator.sheet;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * field scope validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class FieldScopeValidator implements SheetValidator {

  private List<String> fieldScopes = new ArrayList<>();

  public FieldScopeValidator(String... fieldScopes) {
    Collections.addAll(this.fieldScopes, fieldScopes);
  }

  @Override
  public String getErrorMessage() {
    return "有字段不在处理范围内";
  }

  @Override
  public List<ExcelCell> getMessageOnCells(ExcelSheet excelSheet) {
    return Collections.singletonList(excelSheet.getFirstRow().getFirstCell());
  }

  @Override
  public boolean validate(ExcelSheet excelSheet) {
    ExcelRow fieldRow = excelSheet.getRow(2);

    for (ExcelCell fieldCell : fieldRow.getCells()) {
      String field = fieldCell.getValue();
      if (!fieldScopes.contains(field)) {
        return false;
      }
    }

    return true;
  }

}
