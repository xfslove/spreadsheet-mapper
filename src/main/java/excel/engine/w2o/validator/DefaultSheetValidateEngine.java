package excel.engine.w2o.validator;

import excel.engine.model.core.Row;
import excel.engine.model.core.Sheet;
import excel.engine.model.message.ValidateMessage;
import excel.engine.model.meta.FieldMeta;
import excel.engine.model.meta.SheetMeta;
import excel.engine.w2o.processor.ExcelProcessException;
import excel.engine.w2o.validator.cell.CellValidator;
import excel.engine.w2o.validator.row.RowValidator;
import excel.engine.w2o.validator.sheet.SheetValidator;
import org.apache.commons.collections.CollectionUtils;

import java.util.*;

/**
 * Created by hanwen on 15-12-16.
 */
public class DefaultSheetValidateEngine implements SheetValidateEngine {

  private Sheet sheet;

  private SheetMeta sheetMeta;

  /*===============
    validators
   ================*/
  private List<SheetValidator> sheetValidators = new ArrayList<>();
  private Map<String, List<RowValidator>> key2rowValidators = new HashMap<>();
  private Map<String, List<CellValidator>> key2cellValidators = new HashMap<>();

  /*==============
    error messages TODO
   ===============*/
  private List<ValidateMessage> errorMessages = new ArrayList<>();

  @Override
  public SheetValidateEngine sheetValidator(SheetValidator... validators) {
    if (validators == null) {
      return this;
    }
    Collections.addAll(this.sheetValidators, validators);
    return this;
  }

  @Override
  public SheetValidateEngine rowValidator(RowValidator... validators) {
    if (validators == null) {
      return this;
    }

    for (RowValidator validator : validators) {
      String key = validator.getGroup();

      if (!key2rowValidators.containsKey(key)) {
        key2rowValidators.put(key, new ArrayList<RowValidator>());
      }
      key2rowValidators.get(key).add(validator);
    }
    return this;
  }

  @Override
  public SheetValidateEngine cellValidator(CellValidator... validators) {
    if (validators == null) {
      return this;
    }
    for (CellValidator validator : validators) {
      String key = validator.getGroup();

      if (!key2cellValidators.containsKey(key)) {
        key2cellValidators.put(key, new ArrayList<CellValidator>());
      }
      key2cellValidators.get(key).add(validator);
    }
    return this;
  }

  @Override
  public SheetValidateEngine sheet(Sheet sheet) {
    this.sheet = sheet;
    return this;
  }

  @Override
  public SheetValidateEngine sheetMeta(SheetMeta sheetMeta) {
    this.sheetMeta = sheetMeta;
    return this;
  }

  @Override
  public boolean valid() {
    if (sheet == null) {
      throw new ExcelValidateException("sheet is null");
    }

    if (sheetMeta == null) {
      throw new ExcelProcessException("set sheet meta first");
    }

    // check dependency of this sheet
    checkValidatorKeyDependency();

    validSheet(sheet);

    if (CollectionUtils.isNotEmpty(errorMessages)) {
      return false;
    }

    for (Row row : sheet.getRows()) {

      validRowCells(row);
    }

    return CollectionUtils.isEmpty(errorMessages);
  }

  /**
   * check if dependency correct
   */
  private void checkValidatorKeyDependency() {

    // one key corresponding multi validators
    Map<String, List<? extends RelationValidator>> validatorMap = new HashMap<>();
    validatorMap.putAll(key2rowValidators);
    validatorMap.putAll(key2cellValidators);

    Map<String, Set<String>> dependsOnHierarchy = buildDependsOnHierarchy(validatorMap);

    Set<String> satisfiedKeys = new HashSet<>();
    for (String key : dependsOnHierarchy.keySet()) {
      Set<String> dependencyKeys = new HashSet<>();
      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, key);
      satisfiedKeys.addAll(dependencyKeys);
    }
  }

  private void checkValidatorKeyDependencyHierarchy(
      Map<String, Set<String>> dependsOnHierarchy,
      Set<String> satisfiedKeys,
      Set<String> dependencyKeys,
      String key
  ) {

    if (satisfiedKeys.contains(key)) {
      return;
    }

    dependencyKeys.add(key);
    for (String dependsOn : dependsOnHierarchy.get(key)) {

      if (!dependsOnHierarchy.containsKey(dependsOn)) {
        throw new ExcelValidateException("dependency missing key [" + dependsOn + "]");
      }

      if (dependencyKeys.contains(dependsOn)) {
        throw new ExcelValidateException("dependency cycling on [" + key + "] and [" + dependsOn + "]");
      }

      checkValidatorKeyDependencyHierarchy(dependsOnHierarchy, satisfiedKeys, dependencyKeys, dependsOn);
    }
  }

  /*=========================
   below is internal valid
   ==========================*/
  private void validSheet(Sheet sheet) {

    for (SheetValidator validator : sheetValidators) {

      if (!validator.valid(sheet, sheetMeta)) {

      }
    }

  }

  private void validRowCells(Row row) {
    // one key corresponding multi validators
    Map<String, List<? extends RelationValidator>> validatorMap = new HashMap<>();
    validatorMap.putAll(key2rowValidators);
    validatorMap.putAll(key2cellValidators);

    Map<String, Set<String>> dependsOnHierarchy = buildDependsOnHierarchy(validatorMap);

    Map<String, Set<Boolean>> allResult = new HashMap<>();

    for (List<RowValidator> validators : key2rowValidators.values()) {
      for (RowValidator validator : validators) {
        allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getGroup()));
      }
    }
    for (List<CellValidator> validators : key2cellValidators.values()) {
      for (CellValidator validator : validators) {
        allResult.putAll(validRowCellsHierarchy(validatorMap, allResult, dependsOnHierarchy, row, validator.getGroup()));
      }
    }

  }

  private Map<String, Set<Boolean>> validRowCellsHierarchy(
      Map<String, List<? extends RelationValidator>> validatorMap,
      Map<String, Set<Boolean>> allResult,
      Map<String, Set<String>> dependsOnHierarchy,
      Row row,
      String key
  ) {

    Map<String, Set<Boolean>> dependsOnGroupResult = new HashMap<>();

    if (allResult.containsKey(key)) {
      dependsOnGroupResult.put(key, allResult.get(key));
      return dependsOnGroupResult;
    }

    Set<String> dependsOns = dependsOnHierarchy.get(key);

    if (CollectionUtils.isNotEmpty(dependsOns)) {

      for (String dependsOn : dependsOns) {

        dependsOnGroupResult.putAll(validRowCellsHierarchy(validatorMap, dependsOnGroupResult, dependsOnHierarchy, row, dependsOn));
      }

    }

    if (ifSkip(dependsOnGroupResult)) {
      dependsOnGroupResult.put(key, Collections.singleton((Boolean) null));
      return dependsOnGroupResult;
    }

    Set<Boolean> vrs = new HashSet<>();
    for (RelationValidator relationValidator : validatorMap.get(key)) {
      boolean result = doRowCellsValid(relationValidator, row);
      vrs.add(result);
      // if one of the group valid failure skip rest validator
      if (!result) {
        break;
      }
    }
    dependsOnGroupResult.put(key, vrs);
    return dependsOnGroupResult;
  }

  /**
   * if result is not empty and result set not only has true skip the valid
   *
   * @param result dependsOn result
   * @return true if skip
   */
  private boolean ifSkip(Map<String, Set<Boolean>> result) {
    Set<Boolean> dependsOnVrs = new HashSet<>();
    for (Set<Boolean> vr : result.values()) {
      dependsOnVrs.addAll(vr);
    }

    return !dependsOnVrs.isEmpty() && (dependsOnVrs.size() > 1 || !dependsOnVrs.iterator().next());
  }

  private boolean doRowCellsValid(RelationValidator relationValidator, Row row) {
    if (relationValidator instanceof RowValidator) {

      return ((RowValidator) relationValidator).valid(row, sheetMeta);
    } else {

      CellValidator cellValidator = (CellValidator) relationValidator;
      FieldMeta fieldMeta = sheetMeta.getFieldMeta(cellValidator.getMatchField());

      return cellValidator.valid(row.getCell(fieldMeta.getColumnIndex()), fieldMeta);
    }
  }

  private Map<String, Set<String>> buildDependsOnHierarchy(Map<String, List<? extends RelationValidator>> validatorMap) {
    Map<String, Set<String>> dependsOnHierarchy = new HashMap<>();

    for (Map.Entry<String, List<? extends RelationValidator>> entry : validatorMap.entrySet()) {
      String key = entry.getKey();
      dependsOnHierarchy.put(key, new HashSet<String>());

      for (RelationValidator dataValidator : entry.getValue()) {

        Set<String> dependsOn = dataValidator.getDependsOnGroups();
        if (CollectionUtils.isNotEmpty(dependsOn)) {

          dependsOnHierarchy.get(key).addAll(dependsOn);
        }
      }
    }

    return dependsOnHierarchy;
  }
}
