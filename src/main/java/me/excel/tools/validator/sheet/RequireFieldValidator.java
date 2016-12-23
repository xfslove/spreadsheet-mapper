package me.excel.tools.validator.sheet;

import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.model.excel.ExcelSheet;
import org.apache.commons.collections.CollectionUtils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * required field validator
 * <p>
 * Created by hanwen on 4/26/16.
 */
public class RequireFieldValidator implements SheetValidator {

  private List<String> requireFields = new ArrayList<>();

  public RequireFieldValidator(String... requireFields) {
    Collections.addAll(this.requireFields, requireFields);
  }

  @Override
  public String getErrorMessage() {
    return "不包含所有要求的字段";
  }

  @Override
  public List<ExcelCell> getMessageOnCells(ExcelSheet excelSheet) {
    return Collections.singletonList(excelSheet.getFirstRow().getFirstCell());
  }

  @Override
  public boolean validate(ExcelSheet excelSheet) {
    ExcelRow fieldRow = excelSheet.getRow(2);

    List<String> fields = new ArrayList<>();

    for (ExcelCell fieldCell : fieldRow.getCells()) {
      fields.add(fieldCell.getValue());
    }

    return CollectionUtils.subtract(requireFields, fields).isEmpty();

  }

}
