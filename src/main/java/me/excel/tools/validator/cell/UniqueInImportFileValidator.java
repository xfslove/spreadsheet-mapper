package me.excel.tools.validator.cell;

import me.excel.tools.model.excel.ExcelCell;
import org.apache.commons.lang3.StringUtils;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by hanwen on 2016/12/1.
 */
public class UniqueInImportFileValidator extends AbstractCellValidator {

  private Set<String> cellValueHolder = new HashSet<>();

  public UniqueInImportFileValidator(String field) {
    super(field, "导入文件中存在重复数据");
  }

  public UniqueInImportFileValidator(String field, String errorMessage, String prompt) {
    super(field, errorMessage);
  }

  @Override
  protected boolean customValidate(ExcelCell excelCell) {

    String cellValue = excelCell.getValue();
    if (StringUtils.isBlank(cellValue)) {
      return true;
    }

    if (cellValueHolder.contains(cellValue)) {
      return false;
    }

    cellValueHolder.add(cellValue);
    return true;

  }

}
