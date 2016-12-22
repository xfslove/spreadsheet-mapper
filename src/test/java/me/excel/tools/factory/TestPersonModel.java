package me.excel.tools.factory;

import org.joda.time.LocalDate;
import org.joda.time.LocalDateTime;

import java.io.Serializable;

/**
 * Created by hanwen on 2016/12/22.
 */
public class TestPersonModel implements Serializable {

  public static TestPersonModel create() {
    TestPersonModel model = new TestPersonModel();
    model.setAge(18);
    model.setName("Scarlett");
    model.setBirthday(new LocalDate(1984, 11, 22));
    model.setMale(false);
    model.setRegisterTime(new LocalDateTime(2000, 1, 1, 0, 0, 0));
    return model;
  }

  private String name;

  private int age;

  private LocalDate birthday;

  private LocalDateTime registerTime;

  private boolean male;

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public int getAge() {
    return age;
  }

  public void setAge(int age) {
    this.age = age;
  }

  public LocalDate getBirthday() {
    return birthday;
  }

  public void setBirthday(LocalDate birthday) {
    this.birthday = birthday;
  }

  public LocalDateTime getRegisterTime() {
    return registerTime;
  }

  public void setRegisterTime(LocalDateTime registerTime) {
    this.registerTime = registerTime;
  }

  public boolean isMale() {
    return male;
  }

  public void setMale(boolean male) {
    this.male = male;
  }
}
