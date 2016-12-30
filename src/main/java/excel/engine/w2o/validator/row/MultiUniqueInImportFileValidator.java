package excel.engine.w2o.validator.row;

import excel.engine.ExcelConstants;
import excel.engine.model.core.Cell;
import excel.engine.model.core.Row;
import excel.engine.model.meta.FieldMeta;
import excel.engine.model.meta.SheetMeta;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * <pre>
 * value union unique in template validator, it useful when you want valid some cells value union unique.
 *
 * eg:
 * if you excel files has person.idCardNumber and person.idCardType, you will want check if person's identify unique,
 * when the excel files has duplicate person identify this validator will get false.
 * </pre>
 * Created by hanwen on 2016/12/1.
 */
public class MultiUniqueInImportFileValidator extends RowValidatorAdapter {

  // format: "field1:value1,field2:value2,..."
  private Set<String> rowValueHolder = new HashSet<>();

  private Set<String> matchFields = new HashSet<>();

  public MultiUniqueInImportFileValidator(Set<String> matchFields) {
    super("union.unique", "导入文件中存在重复数据", matchFields);
    this.matchFields = matchFields;
  }

  public MultiUniqueInImportFileValidator(String errorMessage, Set<String> matchFields) {
    super("union.unique", errorMessage, matchFields);
    this.matchFields = matchFields;
  }

  @Override
  protected boolean customValidate(Row row, SheetMeta sheetMeta) {

    List<String> holdStringList = new ArrayList<>();


    for (String field : matchFields) {
      FieldMeta fieldMeta = sheetMeta.getFieldMeta(field);
      Cell cell = row.getCell(fieldMeta.getColumnIndex());
      holdStringList.add(buildHoldString(fieldMeta, cell));
    }

    String holdValue = StringUtils.join(holdStringList, ExcelConstants.COMMA_SEPARATOR);

    if (rowValueHolder.contains(holdValue)) {
      return false;
    }

    rowValueHolder.add(holdValue);
    return true;
  }

  /**
   * build cache string as "field:value"
   *
   * @param cell
   * @return
   */
  private String buildHoldString(FieldMeta fieldMeta, Cell cell) {
    return fieldMeta.getName() + ExcelConstants.SEMICOLON_SEPARATOR + cell.getValue();
  }
}
