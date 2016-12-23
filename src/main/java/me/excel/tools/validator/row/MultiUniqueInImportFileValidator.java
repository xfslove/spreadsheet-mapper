package me.excel.tools.validator.row;

import me.excel.tools.ExcelConstants;
import me.excel.tools.model.excel.ExcelCell;
import me.excel.tools.model.excel.ExcelRow;
import me.excel.tools.validator.SkipValidateException;
import org.apache.commons.lang3.StringUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * value union unique in template validator
 * <p>
 * Created by hanwen on 2016/12/1.
 */
public class MultiUniqueInImportFileValidator extends RowValidatorAdapter {

  // 格式为 field1:value1,field2:value2,...
  private Set<String> rowValueHolder = new HashSet<>();

  private List<String> fields = new ArrayList<>();

  public MultiUniqueInImportFileValidator(String[] matchFields) {
    super("导入文件中存在重复数据", matchFields);
    Collections.addAll(this.fields, matchFields);
  }

  public MultiUniqueInImportFileValidator(String errorMessage, String[] matchFields) {
    super(errorMessage, matchFields);
    Collections.addAll(this.fields, matchFields);
  }

  public MultiUniqueInImportFileValidator(String errorMessage, String[] causedByFields, String[] matchFields) {
    super(errorMessage, causedByFields);
    Collections.addAll(this.fields, matchFields);
  }

  @Override
  protected boolean customValidate(ExcelRow excelRow) throws SkipValidateException {

    List<String> holdStringList = fields.stream()
        .map(field -> buildHoldString(excelRow.getCell(field)))
        .collect(Collectors.toList());

    String holdValue = StringUtils.join(holdStringList, ExcelConstants.SEPARATOR);

    if (StringUtils.isBlank(holdValue)) {
      return true;
    }

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
    if (cell == null) {
      return null;
    }
    String value = cell.getValue();
    if (StringUtils.isBlank(value)) {
      return null;
    }
    return cell.getField() + ":" + value;
  }
}
