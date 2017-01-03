package extensible.sheet.w2o.validator.row;

import extensible.sheet.Constants;
import extensible.sheet.model.core.Cell;
import extensible.sheet.model.core.Row;
import extensible.sheet.model.meta.FieldMeta;
import extensible.sheet.model.meta.SheetMeta;
import org.apache.commons.lang3.StringUtils;

import java.util.*;

/**
 * <pre>
 * value union unique validator, it useful when you want valid some cells value union unique.
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

  public MultiUniqueInImportFileValidator(String errorMessage, String[] matchFields) {
    super("row.union.unique", errorMessage, matchFields);
    if (matchFields != null) {
      Collections.addAll(this.matchFields, matchFields);
    }
  }

  @Override
  protected boolean customValidate(Row row, SheetMeta sheetMeta) {

    List<String> holdStringList = new ArrayList<>();

    for (String field : matchFields) {
      FieldMeta fieldMeta = sheetMeta.getFieldMeta(field);
      Cell cell = row.getCell(fieldMeta.getColumnIndex());
      holdStringList.add(buildHoldString(fieldMeta, cell));
    }

    String holdValue = StringUtils.join(holdStringList, Constants.COMMA_SEPARATOR);

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
    return fieldMeta.getName() + Constants.SEMICOLON_SEPARATOR + cell.getValue();
  }
}
