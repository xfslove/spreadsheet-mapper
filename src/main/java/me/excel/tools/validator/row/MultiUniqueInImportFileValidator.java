package me.excel.tools.validator.row;

import me.excel.tools.ExcelConstants;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import org.apache.commons.lang3.StringUtils;

import java.util.*;

/**
 * value union unique in template validator
 * <p>
 * Created by hanwen on 2016/12/1.
 */
public class MultiUniqueInImportFileValidator extends RowValidatorAdapter {

  // 格式为 field1:value1,field2:value2,...
  private Set<String> rowValueHolder = new HashSet<>();

  private List<String> matchFields = new ArrayList<>();

  public MultiUniqueInImportFileValidator(String[] matchFields) {
    super("导入文件中存在重复数据", matchFields);
    Collections.addAll(this.matchFields, matchFields);
  }

  public MultiUniqueInImportFileValidator(String errorMessage, String[] matchFields) {
    super(errorMessage, matchFields);
    Collections.addAll(this.matchFields, matchFields);
  }

  public MultiUniqueInImportFileValidator(String errorMessage, String[] causedByFields, String[] matchFields) {
    super(errorMessage, causedByFields);
    Collections.addAll(this.matchFields, matchFields);
  }

  @Override
  protected boolean customValidate(ExcelRow excelRow) {

    List<String> holdStringList = new ArrayList<>();

    for (String field : matchFields) {
      holdStringList.add(buildHoldString(excelRow.getCell(field)));
    }

    String holdValue = StringUtils.join(holdStringList, ExcelConstants.COMMA_SEPARATOR);

    if (rowValueHolder.contains(holdValue)) {
      return false;
    }

    rowValueHolder.add(holdValue);
    return true;
  }

  /**
   * 缓存中的string 格式为 field:value
   *
   * @param cell
   * @return
   */
  private String buildHoldString(ExcelCell cell) {
    return cell.getField() + ExcelConstants.SEMICOLON_SEPARATOR + cell.getValue();
  }
}
